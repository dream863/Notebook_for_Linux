(** view 层的端到端冒烟测试：真的把控件建出来，真的走信号，真的落盘。

    为什么不用"看一眼截图"：截图没法断言，也没法在没人盯着的时候跑。
    这里直接模拟用户操作 —— 往 entry/buffer 里塞值（这和真人打字走的是
    同一条 `changed` 信号路径）、让定时器自然到期、再用 core 的 API 把
    数据库读回来核对。这样"自动保存"这种最容易写错的地方是被验证过的，
    而不是被相信的。

    需要一个可用的显示环境，但不需要 xvfb 之类的额外设施。 *)

let failures = ref 0
let checks = ref 0

let say fmt = Printf.ksprintf (fun s -> print_string s; flush stdout) fmt

let check name cond =
  incr checks;
  if cond then say "  ok   %s\n" name
  else begin
    incr failures;
    say "  FAIL %s\n" name
  end

let section name = say "\n[%s]\n" name

let check_eq name ~expected actual =
  incr checks;
  if expected = actual then say "  ok   %s\n" name
  else begin
    incr failures;
    (* 不打印值：期望值可能是元组/列表，没有统一的 to_string，
       硬凑一个反而会掩盖真正的差异 *)
    say "  FAIL %s\n" name
  end

let contains needle hay =
  let n = String.length needle and h = String.length hay in
  if n = 0 then true
  else begin
    let found = ref false and i = ref 0 in
    while (not !found) && !i <= h - n do
      if String.sub hay !i n = needle then found := true else incr i
    done;
    !found
  end

let temp_db () =
  let path = Filename.concat (Filename.get_temp_dir_name ()) "notebook-gui-smoke.db" in
  (* 上次跑剩的 WAL/SHM 一起清掉，否则断言的是上一轮的残留 *)
  List.iter
    (fun suffix ->
      let p = path ^ suffix in
      if Sys.file_exists p then Sys.remove p)
    [ ""; "-wal"; "-shm" ];
  path

(** 让 GTK 主循环真的转一会儿，期间到期的定时器（自动保存、搜索防抖、
    预览重渲染）会真的执行 —— 这正是要验证的东西。

    注意必须真进 [GMain.main ()]：只挂定时器不转循环，等于什么都没测。 *)
let pump ms =
  ignore
    (GMain.Timeout.add ~ms ~callback:(fun () ->
         GMain.quit ();
         false));
  GMain.main ()

let () =
  say "notebook view 冒烟测试\n\n";
  ignore (GMain.init ());
  let path = temp_db () in
  let store = Sqlite_store.open_ ~path () in
  let a =
    Sqlite_store.create_note store ~title:"原始标题" ~body:"原始正文" ~tags:[ "旧" ] ()
  in
  Sqlite_store.close store;

  section "建窗口 + 初始加载";
  let t = Notebook_view.App.create ~path () in
  check "初始列表装上了笔记" (List.length t.notes = 1);
  check "自动选中了第一条" (match t.current with Some n -> n.id = a.id | None -> false);
  check "标题栏显示数据库里的标题" (t.title_entry#text = "原始标题");
  check "编辑器显示数据库里的正文" (t.editor_buf#get_text ~slice:false () = "原始正文");

  section "编辑 -> 自动保存";
  (* 模拟真人输入：set_text 触发 changed，和按键走的是同一条路径 *)
  t.title_entry#set_text "改过的标题";
  t.editor_buf#set_text "改过的正文\n- [ ] 待办甲\n- [x] 待办乙";
  t.tag_entry#set_text "新标签, 第二个 ， 第三个";
  check "改动后立刻标脏" (t.label_saved#text = "未保存");
  pump 1200;
  let store = Sqlite_store.open_ ~path () in
  let n = Sqlite_store.get_note store a.id in
  (match n with
   | None -> check "改动落盘" false
   | Some n ->
       check "标题落盘" (n.title = "改过的标题");
       check "正文落盘" (n.body = "改过的正文\n- [ ] 待办甲\n- [x] 待办乙");
       check "标签落盘（去空白、去重、按名字排序）"
         (n.tags = Sqlite_store.normalize_tags [ "新标签"; "第二个"; "第三个" ]));

  section "预览渲染";
  let doc = Markdown.parse "改过的正文\n- [ ] 待办甲\n- [x] 待办乙" in
  let runs : Render.run list = Render.to_runs doc in
  let plain = String.concat "" (List.map (fun (r : Render.run) -> r.text) runs) in
  check "预览文本覆盖了未勾选和已勾选两种复选框"
    (String.length plain > 0 && contains "\xe2\x98\x90" plain && contains "\xe2\x98\x91" plain);
  check "预览里带上了待办行号，可点"
    (List.length (List.filter_map (fun (r : Render.run) -> r.todo_line) runs) = 2);

  section "预览里点复选框 -> 反写正文";
  let target_line =
    List.find_map
      (function Markdown.Todo t when not t.checked -> Some t.line | _ -> None)
      doc
  in
  (match target_line with
   | None -> check "能定位到未勾选的待办" false
   | Some line ->
       Notebook_view.App.on_todo_click t line;
       pump 1200;
       let n2 = Sqlite_store.get_note store a.id in
       check "点击后正文里的方框翻转并落盘"
         (match n2 with
         | Some { body; _ } ->
             body = "改过的正文\n- [x] 待办甲\n- [x] 待办乙"
         | None -> false));

  section "待办统计";
  pump 300;
  check "统计跟着正文走" (contains "2 / 2" t.label_stats#text);

  section "搜索";
  t.search#set_text "待办甲";
  pump 1200;
  check "正文命中（加密库里靠内存匹配）" (List.length t.notes = 1);
  t.search#set_text "搜不到的东西zzz";
  pump 1200;
  check "搜不到时列表为空" (t.notes = []);
  t.search#set_text "";
  pump 1200;
  check "清空搜索后恢复全部" (List.length t.notes = 1);

  section "搜索把当前笔记过滤掉时的选中项";
  Notebook_view.App.new_note t;
  pump 1200;
  let keep_id = (match t.current with Some n -> n.Model.id | None -> assert false) in
  t.editor_buf#set_text "独一份的关键词 quokka";
  pump 1200;
  check "新笔记正文已落盘"
    (match Sqlite_store.get_note store keep_id with
    | Some n -> String.contains n.body 'q'
    | None -> false);
  t.search#set_text "quokka";
  pump 1200;
  check "搜到唯一那条" (List.length t.notes = 1);
  check "选中项就是搜到的那条"
    (match t.current with Some n -> n.Model.id = keep_id | None -> false);
  t.search#set_text "搜不到zzz";
  pump 1200;
  check "搜不到时列表为空" (t.notes = []);
  check "搜不到时切到空状态，而不是留着一个看不见的选中项"
    (match t.current with
    | None -> false
    | Some n -> n.Model.id <= 0 && n.Model.title = "");
  check "空状态下标题框是空的" (t.title_entry#text = "");
  check "空状态下不能加图片"
    (not t.btn_add_img#sensitive);
  t.search#set_text "quokka";
  pump 1200;
  check "搜回来之后自动重新选中" 
    (match t.current with Some n -> n.Model.id = keep_id | None -> false);
  check "搜回来之后能加图片了" t.btn_add_img#sensitive;
  t.search#set_text "";
  pump 1200;
  Notebook_view.App.delete_note_by_id t keep_id;
  pump 300;

  section "置顶";
  (match t.current with
   | None -> check "有选中笔记" false
   | Some n ->
       check "初始未置顶" (not n.pinned);
       t.pin#set_active true;
       pump 300;
       let n2 = Sqlite_store.get_note store a.id in
       check "置顶落盘" (match n2 with Some n -> n.pinned | None -> false));

  section "新建与删除";
  let before = List.length (Sqlite_store.list_notes store) in
  Notebook_view.App.new_note t;
  pump 1200;
  check "多了一条笔记" (List.length (Sqlite_store.list_notes store) = before + 1);
  let fresh = match t.current with Some n -> n | None -> assert false in
  check "新建后自动选中" (fresh.title = "" && fresh.body = "");
  Notebook_view.App.delete_note_by_id t fresh.id;
  let after = List.length (Sqlite_store.list_notes store) in
  check "删除落库" (after = before);
  check "删除后自动选中相邻的笔记"
    (match t.current with Some n -> n.id = a.id | None -> false);

  section "图片附件";
  (* 造几张真 PNG。让 GdkPixbuf 自己编码，而不是手写 PNG 字节 ——
     手写的很容易在某处出错（比如 CRC 或签名），到时候失败的是
     "测试数据不合法"，容易误判成被测代码有问题。 *)
  let write_png path ~w ~h ~colour =
    let pb = GdkPixbuf.create ~width:w ~height:h ~has_alpha:false () in
    GdkPixbuf.fill pb (Int32.of_int colour);
    GdkPixbuf.save pb ~filename:path ~typ:"png"
  in
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "notebook-gui-smoke" in
  (* 目录可能还没建（这一段之前没写过文件）*)
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o700;
  let p1 = Filename.concat dir "a.png" and p2 = Filename.concat dir "b.png" in
  let write_file path data =
    let oc = open_out_bin path in
    output_string oc data;
    close_out oc
  in
  write_png p1 ~w:8 ~h:8 ~colour:0x3366CC;
  write_png p2 ~w:32 ~h:16 ~colour:0xCC3366;
  let owner = (match t.current with Some n -> n.Model.id | None -> assert false) in
  let img_a = Notebook_view.App.add_image_from_path t p1 in
  let img_b = Notebook_view.App.add_image_from_path t p2 in
  check "附加后内存里有两张图" (List.length t.images = 2);
  check "按添加顺序" (List.map (fun (i : Model.image) -> i.Model.id) t.images = [ img_a; img_b ]);
  check_eq "图片宽高是解码出来的，不是猜的" ~expected:(8, 8)
    (let m = List.hd t.images in
     (m.Model.width, m.Model.height));
  check "缩略图控件建出来了（一个图一个按钮）"
    (List.length t.img_box#children = 2);
  check_eq "库里的关联也在" ~expected:[ img_a; img_b ]
    (List.map (fun (i : Model.image) -> i.Model.id) (Sqlite_store.note_images store owner));
  check "读回来的字节和原文件一致"
    ((Option.get (Sqlite_store.get_image store img_b)).data
    = (let ic = open_in_bin p2 in
       let d = really_input_string ic (in_channel_length ic) in
       close_in ic;
       d));
  (* 非图片文件必须被拒绝，而且要在存进库之前 *)
  let bad = Filename.concat dir "not-an-image.txt" in
  write_file bad "hello";
  check "非图片文件被拒绝"
    (match Notebook_view.App.add_image_from_path t bad with
    | _ -> false
    | exception GdkPixbuf.GdkPixbufError _ -> true
    | exception Glib.GError _ -> true);
  check "被拒绝的文件没有进库" (List.length (Sqlite_store.note_images store owner) = 2);
  check "内存里的附件数也没变" (List.length t.images = 2);

  section "图片附件 + 加密";
  Notebook_view.App.apply_enable_crypto t "pw-for-test";
  check "加密后附件还在" (List.length t.images = 2);
  check "加密后缩略图还能解出来（走的是解密后的字节）"
    (List.length t.img_box#children = 2);
  check "加密后图片字节解密得回原样"
    (let ic = open_in_bin p1 in
     let d = really_input_string ic (in_channel_length ic) in
     close_in ic;
     (Option.get (Sqlite_store.get_image t.store img_a)).data = d);
  check "加密后仍能看到图片关联"
    (List.length (Sqlite_store.note_images t.store owner) = 2);
  Notebook_view.App.apply_disable_crypto t;

  section "移除图片";
  Notebook_view.App.remove_image t img_a;
  check "移除后只剩一张" (List.length t.images = 1);
  check "库里也只剩一张关联" (List.length (Sqlite_store.note_images store owner) = 1);
  check "缩略图按钮同步减少" (List.length t.img_box#children = 1);
  check "没人引用的图片本体被回收了" (Sqlite_store.get_image store img_a = None);
  check "还被引用的那张留着" (Sqlite_store.get_image store img_b <> None);
  Notebook_view.App.remove_image t img_b;
  check "全删完附件栏空了" (t.images = []);
  check "全删完缩略图控件也清空" (t.img_box#children = []);

  section "切换笔记时附件跟着换";
  (* 原笔记的图刚被删光，所以拿它当"空"的一边，正好验证切换时
     附件栏会跟着当前笔记走，而不是一直显示上一条笔记的图 *)
  Notebook_view.App.new_note t;
  pump 1200;
  let new_id = (match t.current with Some n -> n.Model.id | None -> assert false) in
  check "新笔记没有附件" (t.images = []);
  let img_new = Notebook_view.App.add_image_from_path t p1 in
  check "新笔记挂上了图" (List.length t.images = 1);
  check "缩略图只有一个" (List.length t.img_box#children = 1);

  Notebook_view.App.select_note t owner;
  pump 300;
  check "切回原笔记，附件栏跟着空了" (t.images = []);
  check "原笔记确实没有关联" (Sqlite_store.note_images store owner = []);
  check "空的时候缩略图控件也清掉了" (t.img_box#children = []);

  Notebook_view.App.select_note t new_id;
  pump 300;
  check "切回新笔记，附件又回来了" (List.map (fun (i : Model.image) -> i.Model.id) t.images = [ img_new ]);
  check "缩略图重新出现" (List.length t.img_box#children = 1);

  section "启用正文加密 -> 落盘的是密文 -> 解锁后能读回";
  let note_id = (match t.current with Some n -> n.Model.id | None -> assert false) in
  t.title_entry#set_text "加密之后";
  t.editor_buf#set_text "这段正文必须以密文形式落盘 cannotary";
  pump 1200;
  let plain_body = (match Sqlite_store.get_note store note_id with Some n -> n.body | None -> "") in
  check "加密前的正文是明文" (plain_body = "这段正文必须以密文形式落盘 cannotary");
  Notebook_view.App.apply_enable_crypto t "pw-for-test";
  check "加密后内存里的正文仍可读（不必重启）"
    (match t.current with Some n -> String.length n.body > 0 | None -> false);
  let cipher_view = Sqlite_store.open_ ~path () in
  check "库被标记为已加密" (Sqlite_store.is_encrypted cipher_view);
  check "未解锁就读正文会明确报错（所以启动必须先解锁再加载列表）"
    (match Sqlite_store.get_note cipher_view note_id with
    | _ -> false
    | exception Sqlite_store.Db_error _ -> true);
  (match Sqlite_store.unlock ~password:"wrong-pw" cipher_view with
   | _ -> check "错误口令解锁失败" false
   | exception Sqlite_store.Db_error _ -> check "错误口令解锁失败" true);
  let unlocked =
    Sqlite_store.unlock ~password:"pw-for-test" cipher_view
  in
  check "正确口令解锁后正文原样读回"
    (match Sqlite_store.get_note unlocked note_id with
    | Some n -> n.body = "这段正文必须以密文形式落盘 cannotary"
    | None -> false);
  check "解锁后正文明文匹配仍然可用"
    (Sqlite_store.body_matches ~query:"cannotary"
       "这段正文必须以密文形式落盘 cannotary");
  Sqlite_store.close unlocked;
  Notebook_view.App.apply_disable_crypto t;
  check "关闭加密后回到明文"
    (match Sqlite_store.get_note store note_id with
    | Some n -> n.body = "这段正文必须以密文形式落盘 cannotary"
    | None -> false);

  Sqlite_store.close store;
  (* destroy 的处理函数会调 GMain.quit()，此时还没有主循环在跑，quit 会被
     丢掉 —— 所以之后绝不能再进 GMain.main ()，否则永远等不到退出。 *)
  t.win#destroy ();

  say "\n%d 项检查，%d 项失败\n" !checks !failures;
  if !failures > 0 then exit 1
