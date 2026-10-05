(** 主窗口：左侧笔记列表 + 右侧标题/标签/编辑·预览。

    视图层的全部可变状态都在下面这个 record 里，没有别的全局。
    三条贯穿全文的约定：

    - **落盘由一层保存定时器兜住。** 标题、标签、正文各自变化都只标脏 +
      排一个 700ms 的保存定时器；切笔记、搜索、关窗口都会先把定时器立刻
      冲掉。没有"忘了保存"的路径，也不会每敲一下键就写一次 SQLite。

    - **[loading] 屏蔽程序化赋值。** 往 entry/buffer 里塞值同样会触发
      `changed` 信号，不挡住就会把刚加载出来的笔记标成"已修改"。

    - **回调一律包 [safely]。** lablgtk3 里未捕获的异常会直接 terminate，
      而 SQLite 异常（唯一可能抛的东西）不该让整个程序消失。 *)

let esc = Markdown.escape_markup

(* ---------- 小工具 ---------- *)

let format_when ts =
  let now = Unix.gettimeofday () in
  let d = now -. ts in
  if d < 60. then "刚刚"
  else if d < 3600. then Printf.sprintf "%d 分钟前" (int_of_float (d /. 60.))
  else if d < 86400. then Printf.sprintf "%d 小时前" (int_of_float (d /. 3600.))
  else
    let tm = Unix.localtime ts in
    let y = tm.Unix.tm_year + 1900
    and mo = tm.Unix.tm_mon + 1
    and dd = tm.Unix.tm_mday in
    if y = (Unix.localtime now).Unix.tm_year + 1900 then
      Printf.sprintf "%d-%02d %02d:%02d" mo dd tm.Unix.tm_hour tm.Unix.tm_min
    else Printf.sprintf "%d-%02d-%02d" y mo dd

(** 全角逗号 U+FF0C 归一成半角。

    不能写成 [if c = '，' then ...]：OCaml 的字符字面量只接受单字节，
    多字节 UTF-8 会被词法器直接判成语法错误。 *)
let normalize_separators s =
  let fullwidth_comma = "\xEF\xBC\x8C" in
  let n = String.length s in
  let b = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    if !i + 3 <= n && String.sub s !i 3 = fullwidth_comma then begin
      Buffer.add_char b ',';
      i := !i + 3
    end
    else begin
      Buffer.add_char b s.[!i];
      incr i
    end
  done;
  Buffer.contents b

(** 标签用逗号分隔，同时接受全角逗号（中文输入法顺手打出来的）。 *)
let parse_tags s =
  let parts = String.split_on_char ',' (normalize_separators s) in
  (* 排序/去重交给 core 的同一份实现，界面里的顺序和库里的顺序才不会打架 *)
  Sqlite_store.normalize_tags parts

(** 空白占位笔记：没有选中任何笔记时右侧显示的东西。 *)
let placeholder_note () =
  Model.make ~id:0 ~title:"" ~body:"" ~tags:[] ~pinned:false ~created_at:0. ~updated_at:0.

(* ---------- 状态 ---------- *)

type t = {
  mutable store : Sqlite_store.t;
  win : GWindow.window;
  cols : GTree.column_list;
  col_id : int GTree.column;
  col_markup : string GTree.column;
  model : GTree.list_store;
  tree : GTree.view;
  search : GEdit.entry;
  status_count : GMisc.label;
  title_entry : GEdit.entry;
  tag_entry : GEdit.entry;
  pin : GButton.toggle_button;
  editor_buf : GText.buffer;
  prev : Preview.t;
  label_stats : GMisc.label;
  label_saved : GMisc.label;
  img_box : GPack.box;  (** GPack.hbox 是函数，类型是 box *)
  img_scroll : GBin.scrolled_window;
  img_hint : GMisc.label;
  btn_add_img : GButton.button;
  mutable images : Model.image list;  (** 当前笔记的附件 *)
  mutable notes : Model.note list;  (** 过滤后、已排序的显示列表 *)
  mutable current : Model.note option;
  mutable loading : bool;  (** 程序化赋值中：屏蔽 changed 导致的标脏 *)
  mutable save_timer : GMain.Timeout.id option;
  mutable render_timer : GMain.Timeout.id option;
  mutable search_timer : GMain.Timeout.id option;
  mutable query : string;
}

let parent t = (t.win :> GWindow.window_skel)

(** 所有信号回调的统一出口。

    异常既往 stderr 打一行、也弹对话框：只在对话框里说"出错了"的话，
    用户没法把细节告诉你，而 SQLite 错误往往得看具体 SQL 才知道问题。 *)
let safely t f =
  try f ()
  with e ->
    prerr_endline ("备忘录内部错误: " ^ Printexc.to_string e);
    Dialogs.report ~parent:(parent t) e

let cancel_save t =
  match t.save_timer with
  | Some id ->
      GMain.Timeout.remove id;
      t.save_timer <- None
  | None -> ()

let cancel_render t =
  match t.render_timer with
  | Some id ->
      GMain.Timeout.remove id;
      t.render_timer <- None
  | None -> ()

let cancel_search t =
  match t.search_timer with
  | Some id ->
      GMain.Timeout.remove id;
      t.search_timer <- None
  | None -> ()

let editor_text t =
  t.editor_buf#get_text ~start:t.editor_buf#start_iter ~stop:t.editor_buf#end_iter
    ~slice:false ()

(* ---------- 核心动作（互相调用，放一个 let rec 组里） ---------- *)

(* ---------- 图片附件 ----------
   图片是笔记的附件，不是正文的一部分：正文要能搜、要能加密，所以保持
   纯 Markdown；图片另存，界面上以缩略图条的形式挂在下方。
   代价是预览里看不到图片内联 —— 那需要把图片字节塞进 TextView，
   而加密库的正文密钥和图片密钥是同一条路，混在一起会让"预览"这条
   路径变得很难推理。 *)

(** 面向用户的失败（不是程序 bug）：文件太大、不是图片之类。

    和"内部错误"分开，是为了让 [pick_image] 能给出具体提示，
    而不是弹一句"出错了"让用户猜。 *)
exception User_error of string

let user_error fmt = Printf.ksprintf (fun s -> raise (User_error s)) fmt

(* 缩略图边长（像素）。 *)
let thumb_size = 72

(* 单张图片的上限。手机照片动辄好几 MB，不挡一下的话误选一个视频文件
   就能让库膨胀几百 MB，而用户毫无察觉。 *)
let max_image_bytes = 20 * 1024 * 1024

(** 从扩展名猜 mime。

    故意不校验扩展名和真实格式是否一致：gdk-pixbuf 是按内容嗅探的，
    显示不受影响；这里猜错只会让 mime 字段不那么准确。 *)
let mime_of_path path =
  let lower = String.lowercase_ascii path in
  let has ext =
    let n = String.length ext in
    let l = String.length lower in
    l > n && String.sub lower (l - n) n = ext
  in
  if has ".png" then "image/png"
  else if has ".jpg" || has ".jpeg" then "image/jpeg"
  else if has ".gif" then "image/gif"
  else if has ".bmp" then "image/bmp"
  else if has ".webp" then "image/webp"
  else if has ".tif" || has ".tiff" then "image/tiff"
  else if has ".svg" then "image/svg+xml"
  else "application/octet-stream"

(** 缩略图：内存字节 -> 临时文件 -> 按边长解码 -> 删临时文件。

    [from_file_at_size] 是"塞进这个框里"而不是"拉成这个尺寸"，
    所以宽图出来是扁的、竖图出来是窄的，比例不会失真。

    lablgtk3 3.1.5 没有从内存字节加载图片的接口（[GdkPixbuf.from_data]
    要的是已解码的像素指针，不是 PNG/JPEG 字节），所以只能过一趟
    临时文件。gdk-pixbuf 按内容嗅探，后缀写成 .img 也没关系。 *)
let load_thumbnail data =
  let path = Filename.temp_file ~temp_dir:(Filename.get_temp_dir_name ()) "notebook-thumb-" ".img" in
  let cleanup () = if Sys.file_exists path then try Sys.remove path with _ -> () in
  try
    let oc = open_out_bin path in
    output_string oc data;
    close_out oc;
    let pb = GdkPixbuf.from_file_at_size path ~width:thumb_size ~height:thumb_size in
    cleanup ();
    pb
  with e ->
    cleanup ();
    raise e

let rec refresh_list t =
  t.notes <- notes_for_query t;
  t.loading <- true;
  ignore (t.model#clear ());
  List.iter
    (fun (n : Model.note) ->
      let iter = t.model#append () in
      t.model#set ~row:iter ~column:t.col_id n.Model.id;
      t.model#set ~row:iter ~column:t.col_markup (row_markup n))
    t.notes;
  t.loading <- false;
  t.status_count#set_text (Printf.sprintf "%d 条笔记" (List.length t.notes));
  (* 列表整体重建后光标会丢，重新指回当前笔记。 *)
  let in_list id = List.exists (fun (n : Model.note) -> n.Model.id = id) t.notes in
  match t.current with
  | Some n when in_list n.Model.id -> select_id t n.Model.id
  | Some _ ->
      (* 当前笔记被过滤掉了（搜索词缩小了范围）。列表里已经没有它，
         却还留着光标/编辑器的话，用户会以为筛选没生效。
         干脆切到第一条；一条都不剩就显示空状态。 *)
      (match t.notes with
       | first :: _ ->
           load_note t first;
           select_id t first.Model.id
       | [] ->
           load_note t (placeholder_note ()))
  | None ->
      (* 还没选中过任何笔记时（比如刚启动）自动选第一条：空编辑器比有内容
         更没用。这里直接 load_note 而不是走 select_note —— 后者会
         回调 save_now，再回调 refresh_list，就成环了。 *)
      (match t.notes with
       | first :: _ ->
           load_note t first;
           select_id t first.Model.id
       | [] -> load_note t (placeholder_note ()))

and notes_for_query t =
  let all = Sqlite_store.list_notes t.store in
  let q = String.trim t.query in
  let picked =
    if q = "" then all
    else
      (* 标题走 FTS5 索引；正文加密后建不了索引，只能拿解密后的内存值匹配 *)
      let title_ids = Sqlite_store.search_titles t.store q in
      List.filter
        (fun (n : Model.note) ->
          List.mem n.Model.id title_ids || Sqlite_store.body_matches ~query:q n.Model.body)
        all
  in
  Model.sort_for_list picked

and row_markup (n : Model.note) =
  let pin = if n.Model.pinned then "<b>★ </b>" else "" in
  let tags =
    match n.Model.tags with
    | [] -> ""
    | ts -> String.concat " " (List.map (fun x -> "#" ^ esc x) ts) ^ "  "
  in
  Printf.sprintf "<b>%s%s</b>\n<small>%s%s</small>" pin (esc (Model.display_title n)) tags
    (format_when n.Model.updated_at)

and select_id t id =
  let found = ref None in
  t.model#foreach (fun _path iter ->
      if t.model#get ~row:iter ~column:t.col_id = id then begin
        found := Some iter;
        false (* 返回 false 中断遍历 *)
      end
      else true);
  match !found with Some iter -> t.tree#selection#select_iter iter | None -> ()

and update_stats t =
  let st = Markdown.stats (Markdown.parse (editor_text t)) in
  t.label_stats#set_text
    (if st.Markdown.total = 0 then "无待办"
     else Printf.sprintf "待办 %d / %d 已完成" st.Markdown.done_ st.Markdown.total)

and schedule_render t =
  cancel_render t;
  t.render_timer <-
    Some
      (GMain.Timeout.add ~ms:180 ~callback:(fun () ->
           t.render_timer <- None;
           safely t (fun () ->
               Preview.render_source t.prev (editor_text t);
               update_stats t);
           true))

and mark_dirty t =
  if (not t.loading) && t.current <> None then begin
    t.label_saved#set_text "未保存";
    cancel_save t;
    t.save_timer <-
      Some
        (GMain.Timeout.add ~ms:700 ~callback:(fun () ->
             t.save_timer <- None;
             safely t (fun () -> save_now t);
             true))
  end

and save_now t =
  cancel_save t;
  match t.current with
  | None -> ()
  | Some n ->
      let title = String.trim t.title_entry#text in
      let body = editor_text t in
      let tags = parse_tags t.tag_entry#text in
      if n.Model.title = title && n.Model.body = body && n.Model.tags = tags then
        t.label_saved#set_text "已保存"
      else begin
        let n' =
          Model.touch (Model.with_tags (Model.with_body (Model.with_title n title) body) tags)
        in
        Sqlite_store.save_note t.store n';
        t.current <- Some n';
        t.label_saved#set_text "已保存";
        (* 标题进 FTS 索引、时间决定排序，列表得跟着重排 *)
        refresh_list t
      end

(** 预览里点复选框。core 只知道"这是第几行的待办"，真要改还是得调
    [Markdown.toggle_todo] 反写源码；改完重渲染，于是勾选在编辑器里
    也看得见 —— 两边永远同源。 *)
and on_todo_click t line =
  let body = editor_text t in
  let body' = Markdown.toggle_todo body line in
  if body' <> body then begin
    t.editor_buf#set_text body';
    Preview.render_source t.prev body';
    update_stats t;
    mark_dirty t
  end

and load_note t (n : Model.note) =
  t.current <- Some n;
  t.loading <- true;
  t.title_entry#set_text n.Model.title;
  t.tag_entry#set_text (String.concat ", " n.Model.tags);
  t.pin#set_active n.Model.pinned;
  t.editor_buf#set_text n.Model.body;
  t.loading <- false;
  Preview.render_source t.prev n.Model.body;
  refresh_images t;
  update_stats t;
  t.label_saved#set_text "已保存"

and select_note t id =
  match t.current with
  | Some n when n.Model.id = id -> ()
  | _ ->
      safely t (fun () ->
          save_now t;
          (* get_note 返回 option（不存在或解密失败都是 None）：
             只有确实读回来了才切换过去 *)
          match Sqlite_store.get_note t.store id with
          | Some n -> load_note t n
          | None ->
              refresh_list t;
              Dialogs.error ~parent:(parent t) ~message:"这条笔记无法读取，可能数据已损坏。" ())

and new_note t =
  safely t (fun () ->
      save_now t;
      let n = Sqlite_store.create_note t.store () in
      t.search#set_text "";
      t.query <- "";
      refresh_list t;
      load_note t n;
      t.title_entry#misc#grab_focus ())

(** 删除的实际动作，不带确认。

    确认框拆到 [delete_current] 里：模态对话框会开嵌套主循环，
    "动作"本身必须能被自动化测试直接调用。 *)
and delete_note_by_id t id =
  safely t (fun () ->
      save_now t;
      Sqlite_store.delete_note t.store id;
      t.current <- None;
      refresh_list t;
      match t.notes with
      | [] -> load_note t (placeholder_note ())
      | first :: _ -> load_note t first)

and delete_current t =
  match t.current with
  | None -> ()
  | Some n ->
      let title = Model.display_title n in
      if
        Dialogs.confirm ~parent:(parent t) ~title:"删除笔记"
          ~question:(Printf.sprintf "删除「%s」？\n这个操作不能撤销。" title) ()
      then delete_note_by_id t n.Model.id

and toggle_pin t =
  match t.current with
  | None -> ()
  | Some n ->
      safely t (fun () ->
          save_now t;
          let pinned = not n.Model.pinned in
          Sqlite_store.set_pinned t.store n.Model.id pinned;
          t.current <- Some (Model.with_pinned (Model.touch n) pinned);
          refresh_list t)

(** 真正把一张文件存成附件的动作。返回新图片的 id。

    模态对话框拆在外面（[pick_image]），所以这条路径能被自动化测试
    直接调用 —— 否则"选文件"就只能靠肉眼验证。 *)
and add_image_from_path t path =
  (* 先让 gdk-pixbuf 真正解一次码：非图片、损坏的文件会在这里抛异常，
     存进去就晚了。 *)
  let pb = GdkPixbuf.from_file path in
  let w = GdkPixbuf.get_width pb and h = GdkPixbuf.get_height pb in
  let size = (Unix.stat path).st_size in
  if size > max_image_bytes then
    user_error "图片有 %d MB，超过 %d MB 的上限" (size / 1024 / 1024)
      (max_image_bytes / 1024 / 1024)
  else begin
    let ic = open_in_bin path in
    let data =
      match really_input_string ic size with
      | d ->
          close_in ic;
          d
      | exception e ->
          close_in_noerr ic;
          raise e
    in
    match t.current with
    | None -> user_error "先选中一条笔记，再附加图片"
    | Some n ->
        let img = Sqlite_store.add_image t.store ~data ~mime:(mime_of_path path) ~width:w ~height:h in
        ignore (Sqlite_store.link_image t.store n.Model.id img.Model.id);
        refresh_images t;
        img.Model.id
  end

(** 摘掉一张附件。图片本体如果没人引用了会被回收掉。 *)
and remove_image t image_id =
  match t.current with
  | None -> ()
  | Some n ->
      safely t (fun () ->
          Sqlite_store.unlink_image t.store n.Model.id image_id;
          ignore (Sqlite_store.gc_orphan_images t.store);
          refresh_images t)

(** 点缩略图：先问一句再删。和删笔记一致 —— 图片误删了就没了，
    而点一下缩略图很容易是无心的。 *)
and confirm_remove_image t image_id =
  let meta = List.find_opt (fun (i : Model.image) -> i.Model.id = image_id) t.images in
  match meta with
  | None -> ()
  | Some m ->
      if
        Dialogs.confirm ~parent:(parent t) ~title:"移除图片"
          ~question:
            (Printf.sprintf "移除这个图片附件？\n\n%s · %d×%d\n\n图片会从数据库里删掉。"
               m.Model.mime m.Model.width m.Model.height)
          ()
      then remove_image t image_id

(** 重建缩略图条。切笔记、增删附件后都要重跑。 *)
and refresh_images t =
  List.iter (fun w -> ignore (t.img_box#remove w)) t.img_box#children;
  let note_id = match t.current with Some n -> Some n.Model.id | None -> None in
  (* 占位笔记（id = 0）不是真笔记：既不用查它的附件，也不该允许往上加 *)
  let real_id = match note_id with Some id when id > 0 -> Some id | _ -> None in
  t.images <-
    (match real_id with None -> [] | Some id -> Sqlite_store.note_images t.store id);
  List.iter
    (fun (img : Model.image) ->
      let label =
        Printf.sprintf "%d×%d · %s\n点击移除这个附件" img.Model.width img.Model.height img.Model.mime
      in
      match Sqlite_store.get_image t.store img.Model.id with
      | None -> ()
      | Some { Model.data; _ } -> (
          match load_thumbnail data with
          | exception e ->
              (* 单张图坏掉不该让整条附件栏消失 *)
              prerr_endline
                ("缩略图加载失败 (image " ^ string_of_int img.Model.id ^ "): "
                ^ Printexc.to_string e)
          | pb ->
              let btn = GButton.button () in
              btn#set_relief `NONE;
              btn#set_tooltip_text label;
              ignore (btn#add ((GMisc.image ~pixbuf:pb ()) :> GObj.widget));
              ignore
                (btn#connect #clicked ~callback:(fun () ->
                     confirm_remove_image t img.Model.id));
              ignore (t.img_box#pack ~from:`START btn#coerce)))
    t.images;
  let has = t.images <> [] in
  t.img_hint#set_visible (not has);
  t.img_scroll#set_visible has;
  t.btn_add_img#set_sensitive (real_id <> None)

(** 「附加图片…」：只有这里用模态对话框，动作本身在
    [add_image_from_path] 里，方便测试。 *)
and pick_image t =
  if t.current <> None then begin
    let dlg =
      GWindow.file_chooser_dialog ~action:`OPEN ~title:"附加图片"
        ~parent:(parent t) ~modal:true ()
    in
    dlg#add_button "取消" `CANCEL;
    dlg#add_select_button "附加" `OK;
    (* 打开前先存：不然刚敲的字会丢 *)
    save_now t;
    let response = dlg#run () in
    let picked = match response with `OK -> dlg#filename | _ -> None in
    dlg#destroy ();
    match picked with
    | None -> ()
    | Some path -> (
        try ignore (add_image_from_path t path)
        with
        | GdkPixbuf.GdkPixbufError (_, msg) ->
            Dialogs.error ~parent:(parent t) ~title:"不是有效的图片" ~message:msg ()
        | Glib.GError msg ->
            Dialogs.error ~parent:(parent t) ~title:"读不了这个文件" ~message:msg ()
        | User_error msg ->
            Dialogs.error ~parent:(parent t) ~title:"附加失败" ~message:msg ()
        | Sqlite_store.Db_error msg ->
            Dialogs.error ~parent:(parent t) ~title:"附加失败" ~message:msg ()
        | Sys_error msg ->
            Dialogs.error ~parent:(parent t) ~title:"附加失败" ~message:msg ()
        | e -> Dialogs.report ~parent:(parent t) e)
  end

and on_search_changed t =
  cancel_search t;
  t.query <- t.search#text;
  t.search_timer <-
    Some
      (GMain.Timeout.add ~ms:180 ~callback:(fun () ->
           t.search_timer <- None;
           safely t (fun () ->
               (* 正文搜索读的是内存里的明文，所以先把改动落盘，
                  否则搜的是"上一个版本"的正文 *)
               save_now t;
               refresh_list t);
           true))

(** 真正启用加密的动作，确认框和口令框拆在外面。

    模态对话框会开嵌套主循环，所以"动作"必须能被自动化测试直接调用，
    否则这条路径就只能靠肉眼验证 —— 而它恰恰是最不能出错的一条。 *)
and apply_enable_crypto t password =
  safely t (fun () ->
      (* PBKDF2 60 万次要跑几百毫秒到一秒，主循环这段时间是卡住的，
         界面"没反应"是预期内的 *)
      t.store <- Sqlite_store.enable_crypto ~password t.store;
      refresh_list t)

and enable_crypto t =
  if
    Dialogs.confirm ~parent:(parent t) ~title:"启用正文加密"
      ~question:
        "将对所有笔记正文和图片启用 AES-256-GCM 加密。\n\n\
         标题、标签、置顶和时间戳仍是明文，否则没法搜索和排序。\n\
         口令只有你自己知道，数据库里没有后门 —— 忘了就取不回来。\n\n继续？"
      ()
  then
    match Dialogs.run_password ~parent:(parent t) ~title:"设置口令" () with
    | Dialogs.Cancelled -> ()
    | Dialogs.Ok pw1 -> (
        match Dialogs.run_password ~parent:(parent t) ~title:"再输一次" () with
        | Dialogs.Cancelled -> ()
        | Dialogs.Ok pw2 when pw1 <> pw2 ->
            Dialogs.error ~parent:(parent t) ~message:"两次输入的口令不一致。" ()
        | Dialogs.Ok pw2 ->
            apply_enable_crypto t pw2;
            Dialogs.info ~parent:(parent t) ~title:"完成" ~message:"正文加密已启用。" ())

(** 真正关闭加密的动作。 *)
and apply_disable_crypto t =
  safely t (fun () ->
      t.store <- Sqlite_store.disable_crypto t.store;
      refresh_list t)

and disable_crypto t =
  if
    Dialogs.confirm ~parent:(parent t) ~title:"关闭正文加密"
      ~question:"将把所有正文和图片解密回明文保存。\n\n继续？" ()
  then begin
    apply_disable_crypto t;
    Dialogs.info ~parent:(parent t) ~title:"完成" ~message:"正文加密已关闭。" ()
  end

(* ---------- 菜单 ---------- *)

let menu_sub items =
  let m = GMenu.menu () in
  List.iter m#append items;
  m

(** 顶层菜单项没有回调（只挂子菜单），所以 [label] 给默认值，
    免得调用处为了消歧义还得显式写 [~label:...]。 *)
let menu_item t ?submenu ?(label = "") cb =
  let it = GMenu.menu_item ~label () in
  ignore (it#connect #activate ~callback:(fun () -> safely t cb));
  (match submenu with Some m -> it#set_submenu m | None -> ());
  it

(* ---------- 组装 ---------- *)
let create ?path () =
  let store = Sqlite_store.open_ ?path () in
  let win = GWindow.window ~title:"备忘录" ~width:1100 ~height:720 () in
  win#set_position `CENTER;

  (* ---------- 列表 ---------- *)
  let cols = new GTree.column_list in
  let col_id = cols#add Gobject.Data.int in
  let col_markup = cols#add Gobject.Data.string in
  let model = GTree.list_store cols in
  let tree = GTree.view ~model ~headers_visible:false () in
  let cell = GTree.cell_renderer_text [] in
  (* 不用 `` `MARKUP true `` 这个 cell property：lablgtk3 3.1.5 把它标成了
     string converter，而 GObject 里 markup 其实是 boolean，设了要么被静默
     忽略要么直接抛 "argument type mismatch"。按属性名绑定才是它自己在
     combo_box_text 里用的路子（见 gEdit.ml）。 *)
  let column = GTree.view_column ~renderer:(cell, []) () in
  column#add_attribute cell "markup" col_markup;
  ignore (tree#append_column column);

  let list_scroll =
    GBin.scrolled_window ~vpolicy:`AUTOMATIC ~hpolicy:`NEVER ~shadow_type:`IN ()
  in
  ignore (list_scroll#add tree#coerce);

  let search = GEdit.entry ~placeholder_text:"搜索标题和正文" () in
  let status_count = GMisc.label ~xalign:0. ~text:"" () in
  let left = GPack.vbox ~spacing:6 ~border_width:8 () in
  ignore (left#pack ~from:`START ~fill:true search#coerce);
  ignore (left#pack ~from:`START ~fill:true ~expand:true list_scroll#coerce);
  ignore (left#pack ~from:`END ~fill:true status_count#coerce);

  (* ---------- 标题 / 标签 ---------- *)
  let title_entry = GEdit.entry ~placeholder_text:"标题（留空则用正文首行）" () in
  let tag_entry = GEdit.entry ~placeholder_text:"标签，用逗号分隔" () in
  let pin = GButton.check_button ~label:"置顶" () in
  let pin_row = GPack.hbox ~spacing:8 ~border_width:8 () in
  ignore (pin_row#pack ~from:`START ~expand:true tag_entry#coerce);
  ignore (pin_row#pack ~from:`START pin#coerce);
  let head = GPack.vbox ~spacing:6 () in
  ignore (head#pack ~from:`START ~fill:true title_entry#coerce);
  ignore (head#pack ~from:`START ~fill:true pin_row#coerce);

  (* ---------- 编辑 / 预览 ---------- *)
  let editor_buf = GText.buffer () in
  let editor = GText.view ~buffer:editor_buf ~wrap_mode:`WORD ~accepts_tab:false () in
  let pad v =
    v#set_left_margin 10;
    v#set_right_margin 10;
    v#set_top_margin 8;
    v#set_bottom_margin 8
  in
  pad editor;
  let prev = Preview.create () in
  pad (Preview.view prev);
  let scroll_of w =
    let s = GBin.scrolled_window ~vpolicy:`AUTOMATIC ~shadow_type:`IN () in
    ignore (s#add w);
    s#coerce
  in
  let tabs = GPack.notebook ~scrollable:true () in
  ignore
    (tabs#append_page ~tab_label:((GMisc.label ~text:"编辑" ())#coerce)
       (scroll_of editor#coerce));
  ignore
    (tabs#append_page ~tab_label:((GMisc.label ~text:"预览" ())#coerce)
       (scroll_of (Preview.view prev)#coerce));

  let label_stats = GMisc.label ~xalign:0. ~text:"" () in
  let label_saved = GMisc.label ~xalign:1. ~text:"" () in
  let status = GPack.hbox ~spacing:8 ~border_width:8 () in
  ignore (status#pack ~from:`START ~expand:true label_stats#coerce);
  ignore (status#pack ~from:`END label_saved#coerce);

  (* ---------- 图片附件条 ----------
     固定高度、横向滚动：附件是补充信息，不该把正文挤没。 *)
  let img_box = GPack.hbox ~spacing:6 ~border_width:6 () in
  let img_scroll =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`NEVER ~height:(thumb_size + 16) ()
  in
  ignore (img_scroll#add img_box#coerce);
  let img_hint = GMisc.label ~xalign:0. ~text:"没有图片附件" () in
  img_hint#set_sensitive false;
  let btn_add_img = GButton.button ~label:"附加图片…" () in
  let img_bar = GPack.hbox ~spacing:6 ~border_width:6 () in
  ignore (img_bar#pack ~from:`START ~expand:true img_hint#coerce);
  ignore (img_bar#pack ~from:`START btn_add_img#coerce);
  let img_row = GPack.vbox ~spacing:0 () in
  ignore (img_row#pack ~from:`START ~fill:true img_bar#coerce);
  ignore (img_row#pack ~from:`START ~fill:true img_scroll#coerce);

  let right = GPack.vbox ~spacing:6 () in
  ignore (right#pack ~from:`START ~fill:true head#coerce);
  ignore (right#pack ~from:`START ~fill:true ~expand:true tabs#coerce);
  ignore (right#pack ~from:`END ~fill:true img_row#coerce);
  ignore (right#pack ~from:`END ~fill:true status#coerce);

  let paned = GPack.paned `HORIZONTAL () in
  paned#pack1 ~resize:false ~shrink:false left#coerce;
  paned#pack2 ~resize:true ~shrink:true right#coerce;
  paned#set_position 320;

  (* ---------- 状态 ---------- *)
  let t =
    {
      store;
      win;
      cols;
      col_id;
      col_markup;
      model;
      tree;
      search;
      status_count;
      title_entry;
      tag_entry;
      pin;
      editor_buf;
      prev;
      label_stats;
      label_saved;
      img_box;
      img_scroll;
      img_hint;
      btn_add_img;
      images = [];
      notes = [];
      current = None;
      loading = true;
      save_timer = None;
      render_timer = None;
      search_timer = None;
      query = "";
    }
  in

  (* ---------- 菜单 ---------- *)
  let about () =
    Dialogs.info ~parent:(parent t) ~title:"关于备忘录"
      ~message:
        "Markdown 备忘录\n\n\
         正文可选 AES-256-GCM 加密（标题、标签、置顶、时间戳仍是明文，\n\
         否则没法搜索和排序）。口令没有后门，忘了就取不回来。\n\n\
         预览由 Pango 富文本渲染，不依赖 WebKit。\n\
         待办的勾选状态直接写在正文里，不单独存一份。" ()
  in
  let bar = GMenu.menu_bar () in
  let top label items =
    bar#append (menu_item t ~label ~submenu:(menu_sub items) (fun () -> ()))
  in
  top "文件"
    [
      menu_item t ~label:"新建" (fun () -> new_note t);
      menu_item t ~label:"立即保存" (fun () -> save_now t);
      menu_item t ~label:"删除这条笔记" (fun () -> delete_current t);
      menu_item t ~label:"附加图片…" (fun () -> pick_image t);
    ];
  top "笔记" [ menu_item t ~label:"置顶 / 取消置顶" (fun () -> toggle_pin t) ];
  top "安全"
    [
      menu_item t ~label:"启用正文加密…" (fun () -> enable_crypto t);
      menu_item t ~label:"关闭正文加密…" (fun () -> disable_crypto t);
    ];
  top "帮助" [ menu_item t ~label:"关于" about ];

  let root = GPack.vbox () in
  ignore (root#pack ~from:`START bar#coerce);
  ignore (root#pack ~from:`START ~fill:true ~expand:true paned#coerce);
  ignore (win#add root#coerce);

  (* ---------- 信号 ---------- *)
  ignore
    (tree#connect #cursor_changed ~callback:(fun () ->
         if not t.loading then
           match t.tree#get_cursor () with
           | Some path, _ ->
               let id = t.model#get ~row:(t.model#get_iter path) ~column:t.col_id in
               safely t (fun () -> select_note t id)
           | (None, _) -> ()));

  ignore (title_entry#connect #changed ~callback:(fun () -> safely t (fun () -> mark_dirty t)));
  ignore (tag_entry#connect #changed ~callback:(fun () -> safely t (fun () -> mark_dirty t)));
  ignore
    (editor_buf#connect #changed ~callback:(fun () ->
         if not t.loading then
           safely t (fun () ->
               mark_dirty t;
               schedule_render t)));
  ignore
    (pin#connect #toggled ~callback:(fun () ->
         (* load_note 会程序化 set_active，靠 loading 挡掉 *)
         if (not t.loading) && t.current <> None then
           let pinned = pin#active in
           match t.current with
           | Some n when n.Model.pinned = pinned -> ()
           | Some n ->
               safely t (fun () ->
                   Sqlite_store.set_pinned t.store n.Model.id pinned;
                   t.current <- Some (Model.with_pinned (Model.touch n) pinned);
                   refresh_list t)
           | None -> ()));
  ignore (search#connect #changed ~callback:(fun () -> safely t (fun () -> on_search_changed t)));
  ignore (btn_add_img#connect #clicked ~callback:(fun () -> pick_image t));
  Preview.set_on_todo_click prev (Some (fun line -> safely t (fun () -> on_todo_click t line)));

  (* destroy 在窗口关闭时一定触发，比 delete-event 省心：
     lablgtk3 3.1.5 的类型化接口没把 delete-event 暴露出来。 *)
  ignore
    (win#connect #destroy ~callback:(fun () ->
         cancel_save t;
         cancel_render t;
         cancel_search t;
         safely t (fun () ->
             save_now t;
             Sqlite_store.close t.store);
         GMain.quit ()));

  t.loading <- false;
  (* 加密库还没解锁时，绝不能读正文、也不能先把窗口亮出来：
     正文解密要用密钥，而密钥只有解锁之后才拿得到。这里先读会直接抛
     "数据库已加密，请先解锁再读取"，而那时解锁对话框还没弹出来，
     整个应用就崩在启动阶段了。所以加密库由 [run] 解锁后再加载。 *)
  if not (Sqlite_store.is_encrypted t.store) then begin
    refresh_list t;
    win#misc#show_all ();
    (* show_all 会把每个控件都点亮，冲掉 refresh_images 里
       "没有附件就藏起来"的设置，所以这里要再刷一次。 *)
    refresh_images t
  end;
  t

let run ?path () =
  ignore (GMain.init ());
  let t = create ?path () in
  if Sqlite_store.is_encrypted t.store then begin
    (* 口令错三次就退出：无限重试会把"忘了口令"变成无限等待 *)
    let rec loop left =
      if left <= 0 then false
      else
        match Dialogs.run_password ~title:"解锁备忘录" () with
        | Dialogs.Cancelled -> false
        | Dialogs.Ok pw -> (
            match Sqlite_store.unlock ~password:pw t.store with
            | s ->
                t.store <- s;
                true
            | exception Sqlite_store.Db_error _ ->
                Dialogs.error ~title:"解锁失败" ~message:"口令错误。" ();
                loop (left - 1))
    in
    if not (loop 3) then begin
      Sqlite_store.close t.store;
      false
    end
    else begin
      refresh_list t;
      t.win#misc#show_all ();
      GMain.main ();
      true
    end
  end
  else begin
    GMain.main ();
    true
  end
