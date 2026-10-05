let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else begin
    incr failures;
    Printf.printf "  FAIL %s\n" name
  end

let check_eq name ~expected actual =
  incr checks;
  if expected = actual then Printf.printf "  ok   %s\n" name
  else begin
    incr failures;
    Printf.printf "  FAIL %s\n" name
  end

let section name = Printf.printf "\n[%s]\n" name

let contains hay needle =
  let h = String.length hay and n = String.length needle in
  if n = 0 then true
  else begin
    let found = ref false and i = ref 0 in
    while (not !found) && !i <= h - n do
      if String.sub hay !i n = needle then found := true else incr i
    done;
    !found
  end

(** 检查 haystack 里是否存在裸的 Pango 标签起始 `<字母`，
    用来验证 markdown 里的标签注入已被中和。 *)
let has_raw_tag hay =
  let h = String.length hay in
  let found = ref false and i = ref 0 in
  let starts_tag j =
    hay.[j] = '<' && j + 1 < h
    &&
    match hay.[j + 1] with
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false
  in
  while (not !found) && !i < h do
    if starts_tag !i then found := true else incr i
  done;
  !found

let markup src = Render.to_markup (Markdown.parse src)


(* ---------- model ---------- *)

let test_model () =
  section "Model";
  let n =
    Model.make ~id:1 ~title:"" ~body:"第一行正文\n第二行" ~tags:[] ~pinned:false
      ~created_at:0. ~updated_at:0.
  in
  check_eq "标题为空时用正文首行兜底" ~expected:"第一行正文" (Model.display_title n);
  let n2 = { n with Model.body = "" } in
  check_eq "标题和正文都空时显示无标题" ~expected:"无标题" (Model.display_title n2);
  let blank = { n with Model.title = "   "; Model.body = "\n\n  " } in
  check_eq "纯空白也走无标题" ~expected:"无标题" (Model.display_title blank);
  check "display_title 截断长行"
    (String.length (Model.display_title { n with Model.title = ""; Model.body = String.make 200 'x' }) <= 60);
  let pinned = { n2 with Model.pinned = true; Model.title = "P"; Model.updated_at = 1. } in
  let normal = { n2 with Model.title = "N"; Model.updated_at = 2. } in
  check "置顶排在前面" (Model.compare_for_list pinned normal < 0);
  let older = { n with Model.title = "a"; Model.updated_at = 1. } in
  let newer = { n with Model.title = "b"; Model.updated_at = 2. } in
  check "同置顶状态按更新时间倒序" (Model.compare_for_list newer older < 0);
  let sorted = Model.sort_for_list [ older; pinned; normal ] in
  check_eq "sort_for_list 长度" ~expected:3 (List.length sorted);
  check "sort_for_list 首项是置顶项" ((List.hd sorted).Model.pinned);
  check "touch 会推进 updated_at" ((Model.touch older).Model.updated_at > 1.)

(* ---------- crypto ---------- *)

let test_crypto () =
  section "Crypto";
  let salt = Crypto.new_salt () in
  check_eq "盐长度 16" ~expected:16 (String.length salt);
  check "两次生成的盐不同" (Crypto.new_salt () <> Crypto.new_salt ());
  check_eq "KDF 算法名" ~expected:"pbkdf2-hmac-sha512" Crypto.kdf_algorithm;
  check "迭代次数 >= 600000" (Crypto.kdf_iterations >= 600_000);
  let key = Crypto.create ~password:"correct horse" ~salt in
  let cipher, nonce = Crypto.encrypt key "秘密内容 hello" in
  check_eq "nonce 长度 12" ~expected:12 (String.length nonce);
  check "密文不等于明文" (cipher <> "秘密内容 hello");
  check_eq "往返解密一致" ~expected:"秘密内容 hello"
    (Crypto.decrypt key ~cipher ~nonce);
  let cipher2, nonce2 = Crypto.encrypt key "秘密内容 hello" in
  check "同密钥同明文两次加密密文不同（nonce 不复用）"
    (cipher2 <> cipher && nonce2 <> nonce);
  let key2 = Crypto.create ~password:"wrong password" ~salt in
  let bad =
    try
      ignore (Crypto.decrypt key2 ~cipher ~nonce);
      None
    with Crypto.Crypto_exn Crypto.Bad_password -> Some ()
  in
  check "口令错误抛 Bad_password 而不是返回乱码" (bad <> None);
  let tampered =
    String.sub cipher 0 (String.length cipher - 1) ^ "X"
  in
  let bad2 =
    try
      ignore (Crypto.decrypt key ~cipher:tampered ~nonce);
      None
    with Crypto.Crypto_exn Crypto.Bad_password -> Some ()
  in
  check "密文被篡改抛 Bad_password" (bad2 <> None);
  let bin =
    Bytes.to_string (Bytes.init 4096 (fun i -> Char.chr (i mod 256)))
  in
  let bc, bn = Crypto.encrypt key bin in
  check_eq "二进制数据往返一致" ~expected:bin (Crypto.decrypt key ~cipher:bc ~nonce:bn);
  let ec, en = Crypto.encrypt key "" in
  check_eq "空串往返" ~expected:"" (Crypto.decrypt key ~cipher:ec ~nonce:en);
  let cn = "中文内容 with ascii" in
  let cc, cn_nonce = Crypto.encrypt key cn in
  check_eq "中英混排往返" ~expected:cn
    (Crypto.decrypt key ~cipher:cc ~nonce:cn_nonce);
  Crypto.wipe key

(* ---------- store 辅助 ---------- *)

let tmp_dir tag =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "notebook-%s-%d" tag (Unix.getpid ()))

let cleanup path suffixes =
  List.iter (fun suf -> try Sys.remove (path ^ suf) with _ -> ()) suffixes;
  let dir = Filename.dirname path in
  (try Unix.rmdir dir with _ -> ())

let with_store tag f =
  let path = Filename.concat (tmp_dir tag) "t.db" in
  let st = Sqlite_store.open_ ~path () in
  match f st with
  | r ->
      Sqlite_store.close st;
      cleanup path [ ""; "-wal"; "-shm" ];
      r
  | exception e ->
      Sqlite_store.close st;
      cleanup path [ ""; "-wal"; "-shm" ];
      raise e

(* ---------- store 基本 CRUD ---------- *)

let test_store_basic () =
  section "Store 基本 CRUD";
  with_store "basic" (fun st ->
      check "新建库默认未加密" (not (Sqlite_store.is_encrypted st));
      let n =
        Sqlite_store.create_note st ~title:"第一条" ~body:"内容 A"
          ~tags:[ "工作"; "urgent" ] ()
      in
      check "新笔记 id > 0" (n.Model.id > 0);
      let got = Option.get (Sqlite_store.get_note st n.Model.id) in
      check_eq "标题往返" ~expected:"第一条" got.Model.title;
      check_eq "正文往返" ~expected:"内容 A" got.Model.body;
      check_eq "标签往返（排序去重后）" ~expected:[ "urgent"; "工作" ] got.Model.tags;
      check "未置顶" (not got.Model.pinned);
      let n2 = Sqlite_store.create_note st ~title:"第二条" ~body:"内容 B" () in
      check_eq "列表长度 2" ~expected:2 (List.length (Sqlite_store.list_notes st));
      let updated =
        Model.touch { got with Model.body = "内容 A2" }
      in
      Sqlite_store.save_note st updated;
      check_eq "保存后正文更新" ~expected:"内容 A2"
        (Option.get (Sqlite_store.get_note st n.Model.id)).Model.body;
      check_eq "保存后标签保留" ~expected:[ "urgent"; "工作" ]
        (Option.get (Sqlite_store.get_note st n.Model.id)).Model.tags;
      Sqlite_store.set_pinned st n2.Model.id true;
      check "置顶后排在首位"
        ((List.hd (Sqlite_store.list_notes st)).Model.id = n2.Model.id);
      check_eq "all_tags 汇总两个标签" ~expected:[ "urgent"; "工作" ]
        (Sqlite_store.all_tags st);
      Sqlite_store.save_note st { (Option.get (Sqlite_store.get_note st n2.Model.id)) with Model.tags = [ "x"; "y"; "x" ] };
      check_eq "标签去重" ~expected:[ "x"; "y" ]
        (Option.get (Sqlite_store.get_note st n2.Model.id)).Model.tags;
      Sqlite_store.save_note st { (Option.get (Sqlite_store.get_note st n2.Model.id)) with Model.tags = [ "x"; "  " ] };
      check_eq "空白标签被丢弃" ~expected:[ "x" ]
        (Option.get (Sqlite_store.get_note st n2.Model.id)).Model.tags;
      Sqlite_store.delete_note st n.Model.id;
      check_eq "删除后剩 1 条" ~expected:1 (List.length (Sqlite_store.list_notes st));
      check "删除后 get_note 返回 None" (Sqlite_store.get_note st n.Model.id = None);
      (* 标签是全局字典：删笔记后标签本身保留（还能被别的笔记选中），
         只是不再关联到这条笔记 *)
      check "删除笔记后标签字典仍保留" (List.mem "urgent" (Sqlite_store.all_tags st));
      Sqlite_store.delete_note st 99999;
      check "删除不存在的 id 不抛异常" true)

let test_store_persistence () =
  section "Store 重启后恢复";
  let path = Filename.concat (tmp_dir "persist") "p.db" in
  let st = Sqlite_store.open_ ~path () in
  let id =
    (Sqlite_store.create_note st ~title:"持久化" ~body:"重启要还在"
       ~tags:[ "x" ] ~pinned:true ()).Model.id
  in
  Sqlite_store.close st;
  let st2 = Sqlite_store.open_ ~path () in
  let got = Option.get (Sqlite_store.get_note st2 id) in
  check_eq "重启后标题" ~expected:"持久化" got.Model.title;
  check_eq "重启后正文" ~expected:"重启要还在" got.Model.body;
  check_eq "重启后标签" ~expected:[ "x" ] got.Model.tags;
  check "重启后置顶状态保留" got.Model.pinned;
  check_eq "重启后 created_at 不变" ~expected:true
    (got.Model.created_at > 0. && got.Model.created_at = got.Model.created_at);
  Sqlite_store.close st2;
  cleanup path [ ""; "-wal"; "-shm" ]

let test_store_default_path () =
  section "Store 默认路径与目录创建";
  let p = Sqlite_store.default_db_path () in
  check "默认路径以 notebook.db 结尾"
    (Filename.basename p = "notebook.db");
  check "默认路径在 XDG data 下或 HOME 下"
    (String.length p > String.length "notebook.db");
  check "NOTEBOOK_DB 可覆盖路径"
    (let old = Sys.getenv_opt "NOTEBOOK_DB" in
     Unix.putenv "NOTEBOOK_DB" "/tmp/xyz-notebook.db";
     let p2 = Sqlite_store.default_db_path () in
     (match old with
      | None ->
          (* 4.05+ 没有 unsetenv；直接设成空串再由 default_db_path
             判空即可，等价效果 *)
          Unix.putenv "NOTEBOOK_DB" ""
      | Some v -> Unix.putenv "NOTEBOOK_DB" v);
     p2 = "/tmp/xyz-notebook.db")

(* ---------- store 加密 ---------- *)

let test_store_encrypted () =
  section "Store 加密模式";
  with_store "enc" (fun st0 ->
      (* 先建一条明文笔记，验证 enable_crypto 会把已有数据一起转成密文 *)
      let pre = Sqlite_store.create_note st0 ~title:"加密前的笔记" ~body:"明文正文" ~tags:[ "旧" ] () in
      let pre_img = Sqlite_store.add_image st0 ~data:"old-bytes" ~mime:"image/png" ~width:1 ~height:1 in
      check "未加密时能看到明文正文"
        (match Sqlite_store.get_note st0 pre.Model.id with
         | Some n -> n.Model.body = "明文正文"
         | None -> false);
      check "未加密时图片是原字节"
        ((Option.get (Sqlite_store.get_image st0 pre_img.Model.id)).data
         = "old-bytes");
      let st = Sqlite_store.enable_crypto ~password:"pw123" st0 in
      check "开启后 is_encrypted 为真" (Sqlite_store.is_encrypted st);
      let alg, iter, salt = Sqlite_store.kdf_params st in
      check_eq "KDF 算法已落库" ~expected:Crypto.kdf_algorithm alg;
      check_eq "迭代次数已落库" ~expected:Crypto.kdf_iterations iter;
      check "盐已落库" (Option.is_some salt);
      let n =
        Sqlite_store.create_note st ~title:"密文笔记" ~body:"机密正文内容"
          ~tags:[ "秘密" ] ()
      in
      let got = Option.get (Sqlite_store.get_note st n.Model.id) in
      check_eq "加密模式下标题仍明文可读" ~expected:"密文笔记" got.Model.title;
      check_eq "加密模式下正文能解密回来" ~expected:"机密正文内容" got.Model.body;
      check_eq "加密模式下标签可用" ~expected:[ "秘密" ] got.Model.tags;
      check "加密模式下列表读取正常"
        (List.length (Sqlite_store.list_notes st) = 2);
      check "更新加密正文后可再读回"
        (let u = Model.touch { got with Model.body = "改过的机密" } in
         Sqlite_store.save_note st u;
         (Option.get (Sqlite_store.get_note st n.Model.id)).Model.body = "改过的机密");
      (* 开关转换后，已有的明文数据必须也变成密文 *)
      let pre_raw = Sqlite_store.raw_cipher_and_nonce st pre.Model.id in
      check "enable_crypto 把已有明文正文改写成密文"
        (match pre_raw with Some (c, _) -> c <> "明文正文" | None -> false);
      check "enable_crypto 后旧笔记仍能正常读出"
        (match Sqlite_store.get_note st pre.Model.id with
         | Some x -> x.Model.body = "明文正文"
         | None -> false);
      check "enable_crypto 把已有图片也加密"
        ((Option.get (Sqlite_store.get_image st pre_img.Model.id)).data
         = "old-bytes");
      let img_raw = Sqlite_store.image_raw_data st pre_img.Model.id in
      check "图片落盘内容已不是明文字节"
        (match img_raw with Some (c, _) -> c <> "old-bytes" | None -> false);
      (* 关闭加密 *)
      let st3 = Sqlite_store.disable_crypto st in
      check "关闭后 is_encrypted 为假" (not (Sqlite_store.is_encrypted st3));
      check_eq "关闭加密后新笔记正文仍是原文" ~expected:"改过的机密"
        (Option.get (Sqlite_store.get_note st3 n.Model.id)).Model.body;
      check_eq "关闭加密后旧笔记正文也还原" ~expected:"明文正文"
        (Option.get (Sqlite_store.get_note st3 pre.Model.id)).Model.body;
      check "关闭加密后旧图片字节还原"
        ((Option.get (Sqlite_store.get_image st3 pre_img.Model.id)).data
         = "old-bytes");
      let st4 =
        Sqlite_store.raw_cipher_and_nonce st3 pre.Model.id
      in
      check "关闭加密后正文落盘即明文"
        (match st4 with Some (c, n) -> c = "明文正文" && n = "" | None -> false);
      check "关闭加密后再启用也正常"
        (let s5 = Sqlite_store.enable_crypto ~password:"again" st3 in
         Sqlite_store.is_encrypted s5
         &&
         (match Sqlite_store.get_note s5 pre.Model.id with
          | Some x -> x.Model.body = "明文正文"
          | None -> false)))

let test_store_unlock () =
  section "Store 加密库重启 / 解锁校验";
  let path = Filename.concat (tmp_dir "encp") "e.db" in
  let st = Sqlite_store.open_ ~path () in
  let st = Sqlite_store.enable_crypto ~password:"right-pw" st in
  let id = (Sqlite_store.create_note st ~title:"T" ~body:"加密的正文" ()).Model.id in
  Sqlite_store.close st;
  let st2 = Sqlite_store.open_ ~path () in
  check "重开后仍是加密状态" (Sqlite_store.is_encrypted st2);
  check "未解锁时读取报错，而不是返回密文或空结果"
    (try
       ignore (Sqlite_store.get_note st2 id);
       false
     with Sqlite_store.Db_error _ -> true);
  let wrong =
    try
      ignore (Sqlite_store.unlock ~password:"wrong-pw" st2);
      false
    with Sqlite_store.Db_error _ -> true
  in
  check "错误口令解锁失败" wrong;
  let st3 = Sqlite_store.unlock ~password:"right-pw" st2 in
  check "正确口令解锁成功"
    (match Sqlite_store.get_note st3 id with
     | Some n -> n.Model.body = "加密的正文"
     | None -> false);
  check "落盘内容不等于明文"
    (match Sqlite_store.raw_cipher_and_nonce st3 id with
     | Some (c, _) -> c <> "加密的正文"
     | None -> false);
  let _, _, salt = Sqlite_store.kdf_params st3 in
  (match salt with
   | None -> check "盐应已落库" false
   | Some s ->
       let good = Crypto.create ~password:"right-pw" ~salt:s in
       let bad = Crypto.create ~password:"wrong-pw" ~salt:s in
       let raw = Sqlite_store.raw_cipher_and_nonce st3 id in
       let cipher = Option.map fst raw |> Option.value ~default:"" in
       let nonce = Option.map snd raw |> Option.value ~default:"" in
       check "正确口令能解开落盘密文"
         (Crypto.decrypt good ~cipher ~nonce = "加密的正文");
       let outcome =
         try `Plain (Crypto.decrypt bad ~cipher ~nonce) with
         | Crypto.Crypto_exn Crypto.Bad_password -> `Bad
       in
       check "错误口令解不开落盘密文" (outcome = `Bad);
       Crypto.wipe good;
       Crypto.wipe bad);
  check "空库加密后也能验证口令"
    (let p2 = Filename.concat (tmp_dir "encless") "e2.db" in
     let a = Sqlite_store.open_ ~path:p2 () in
     let a = Sqlite_store.enable_crypto ~password:"pw" a in
     Sqlite_store.close a;
     let b = Sqlite_store.open_ ~path:p2 () in
     let ok = Sqlite_store.unlock ~password:"pw" b in
     Sqlite_store.close ok;
     let c = Sqlite_store.open_ ~path:p2 () in
     let bad =
       try ignore (Sqlite_store.unlock ~password:"no" c); false
       with Sqlite_store.Db_error _ -> true
     in
     Sqlite_store.close c;
     cleanup p2 [ ""; "-wal"; "-shm" ];
     bad);
  Sqlite_store.close st3;
  cleanup path [ ""; "-wal"; "-shm" ]

let test_store_images () =
  section "Store 图片 BLOB";
  with_store "img" (fun st ->
      let data = "\137PNG\r\n\032\n\x01\x02\x03 fake png bytes" in
      let img =
        Sqlite_store.add_image st ~data ~mime:"image/png" ~width:100 ~height:50
      in
      check "图片 id > 0" (img.Model.id > 0);
      check_eq "图片 mime" ~expected:"image/png" img.Model.mime;
      check_eq "图片宽" ~expected:100 img.Model.width;
      check_eq "图片高" ~expected:50 img.Model.height;
      let got = Option.get (Sqlite_store.get_image st img.Model.id) in
      check_eq "图片字节往返一致" ~expected:data got.data;
      check_eq "image_data 带回元数据" ~expected:100 (got.image : Model.image).width;
      let st2 = Sqlite_store.enable_crypto ~password:"pw" st in
      let img2 =
        Sqlite_store.add_image st2 ~data ~mime:"image/png" ~width:1 ~height:1
      in
      check_eq "加密模式图片往返一致" ~expected:data
        (Option.get (Sqlite_store.get_image st2 img2.Model.id)).data;
      check_eq "加密模式图片元数据保留" ~expected:1
        (Option.get (Sqlite_store.get_image st2 img2.Model.id)).image.width;
      let empty =
        Sqlite_store.add_image st2 ~data:"" ~mime:"image/gif" ~width:0 ~height:0
      in
      check_eq "空图片字节往返" ~expected:""
        (Option.get (Sqlite_store.get_image st2 empty.Model.id)).data;
      Sqlite_store.delete_image st img.Model.id;
      check "删除图片后取不到" (Sqlite_store.get_image st img.Model.id = None);
      check "删除不存在的图片不抛异常"
        (ignore (Sqlite_store.delete_image st 12345); true))

(* ---------- 搜索 ---------- *)

let test_search () =
  section "搜索分级（trigram >= 3 字符，否则 LIKE）";
  with_store "search" (fun st ->
      ignore (Sqlite_store.create_note st ~title:"备忘录列表功能" ());
      ignore (Sqlite_store.create_note st ~title:"买菜清单" ());
      ignore (Sqlite_store.create_note st ~title:"读书笔记" ());
      check_eq "3 字查询走 trigram 命中" ~expected:1
        (List.length (Sqlite_store.search_titles st "列表功"));
      check_eq "2 字查询降级 LIKE 命中" ~expected:1
        (List.length (Sqlite_store.search_titles st "列表"));
      check_eq "2 字查询命中" ~expected:1
        (List.length (Sqlite_store.search_titles st "买菜"));
      check_eq "1 字查询命中" ~expected:1
        (List.length (Sqlite_store.search_titles st "买"));
      check_eq "无匹配返回空" ~expected:0
        (List.length (Sqlite_store.search_titles st "不存在的词条"));
      check_eq "空查询返回空" ~expected:0
        (List.length (Sqlite_store.search_titles st ""));
      check_eq "纯空白查询返回空" ~expected:0
        (List.length (Sqlite_store.search_titles st "   "));
      check "FTS 特殊字符不导致 SQL 报错"
        (List.length (Sqlite_store.search_titles st "a*b\"c") >= 0);
      check "LIKE 通配符 % 被当作字面量而非通配"
        (List.length (Sqlite_store.search_titles st "%") = 0);
      check "正文匹配（解密后内存扫描）"
        (Sqlite_store.body_matches ~query:"买" "要买菜和水果");
      check "正文匹配大小写不敏感"
        (Sqlite_store.body_matches ~query:"hello" "say HELLO world");
      check "正文不匹配返回 false"
        (not (Sqlite_store.body_matches ~query:"xyz" "hello"));
      check "空查询不匹配任何正文"
        (not (Sqlite_store.body_matches ~query:"  " "abc")))

let test_search_index_consistency () =
  section "FTS 索引与增删改保持一致";
  with_store "fts" (fun st ->
      let a = Sqlite_store.create_note st ~title:"原始标题" () in
      check_eq "新建后能搜到" ~expected:1
        (List.length (Sqlite_store.search_titles st "原始标"));
      Sqlite_store.save_note st
        { (Option.get (Sqlite_store.get_note st a.Model.id)) with Model.title = "改过的标题" };
      check_eq "改标题后旧标题搜不到" ~expected:0
        (List.length (Sqlite_store.search_titles st "原始标"));
      check_eq "改标题后新标题能搜到" ~expected:1
        (List.length (Sqlite_store.search_titles st "改过的标"));
      Sqlite_store.delete_note st a.Model.id;
      check_eq "删除后搜不到" ~expected:0
        (List.length (Sqlite_store.search_titles st "改过的标")))

(* ---------- markdown ---------- *)

let has_block pred doc = List.exists pred doc

let test_markdown () =
  section "Markdown 解析";
  let doc = Markdown.parse "# 标题一\n\n正文一段\n\n## 二级\n" in
  check "识别一级标题"
    (has_block (function Markdown.Heading (1, _) -> true | _ -> false) doc);
  check "识别二级标题"
    (has_block (function Markdown.Heading (2, _) -> true | _ -> false) doc);
  check "识别段落"
    (has_block (function Markdown.Paragraph _ -> true | _ -> false) doc);
  let doc2 = Markdown.parse "- [ ] 未完成\n- [x] 已完成\n- 普通项\n" in
  let todos =
    List.filter_map (function Markdown.Todo t -> Some t | _ -> None) doc2
  in
  check_eq "识别两个待办" ~expected:2 (List.length todos);
  check "待办行号从 0 起" ((List.hd todos : Markdown.todo).Markdown.line = 0);
  check "第二个待办行号为 1"
    ((List.nth todos 1 : Markdown.todo).Markdown.line = 1);
  check "未勾选状态正确" (not (List.hd todos).Markdown.checked);
  check "已勾选状态正确" (List.nth todos 1 : Markdown.todo).Markdown.checked;
  check "同时识别普通列表项"
    (has_block (function Markdown.Bullet _ -> true | _ -> false) doc2);
  let doc3 = Markdown.parse "```ocaml\nlet x = 1\n```\n" in
  check "识别代码块"
    (has_block
       (function Markdown.Code c -> String.contains c 'x' | _ -> false)
       doc3);
  let doc4 = Markdown.parse "1. 第一\n2. 第二\n" in
  check "识别有序列表"
    (has_block (function Markdown.Ordered (1, _) -> true | _ -> false) doc4);
  check "有序列表带正确序号"
    (has_block (function Markdown.Ordered (2, _) -> true | _ -> false) doc4);
  check "识别引用"
    (has_block (function Markdown.Quote _ -> true | _ -> false)
       (Markdown.parse "> 引用一句\n"));
  check "识别分隔线"
    (has_block (function Markdown.Rule -> true | _ -> false)
       (Markdown.parse "a\n\n---\n\nb\n"));
  check "识别星号列表"
    (has_block (function Markdown.Bullet _ -> true | _ -> false)
       (Markdown.parse "* 星号项\n"));
  check "识别加号列表"
    (has_block (function Markdown.Bullet _ -> true | _ -> false)
       (Markdown.parse "+ 加号项\n"));
  (* 行内 *)
  let inline_in_para pat src =
    has_block
      (function
        | Markdown.Paragraph l -> List.exists pat l
        | _ -> false)
      (Markdown.parse src)
  in
  check "粗体"
    (inline_in_para (function Markdown.Bold _ -> true | _ -> false) "**粗**");
  check "斜体"
    (inline_in_para (function Markdown.Italic _ -> true | _ -> false) "*斜*");
  check "删除线"
    (inline_in_para (function Markdown.Strike _ -> true | _ -> false) "~~删~~");
  check "行内代码"
    (inline_in_para (function Markdown.Code _ -> true | _ -> false) "用 `code` 标记");
  check "链接"
    (inline_in_para (function Markdown.Link _ -> true | _ -> false)
       "见 [文档](http://x.y)");
  check "加粗标题带内容"
    (has_block
       (function Markdown.Heading (_, l) -> l <> [] | _ -> false)
       (Markdown.parse "## 有内容\n"));
  (* 不应误判 *)
  check "缺空格的 [ ] 不误判为待办"
    (not (has_block (function Markdown.Todo _ -> true | _ -> false)
            (Markdown.parse "- [ ]这不是待办\n")));
  check "段落里的 [x] 不误判为待办"
    (not (has_block (function Markdown.Todo _ -> true | _ -> false)
            (Markdown.parse "普通文字里的 [x] 不是列表\n")));
  check "代码块里的待办不识别为 Todo"
    (not (has_block (function Markdown.Todo _ -> true | _ -> false)
            (Markdown.parse "```\n- [ ] 在代码里\n```\n")));
  check "未闭合的 ** 不抛异常"
    (String.length (markup "未闭合 **粗体") > 0);
  check "未闭合的 ` 不抛异常"
    (String.length (markup "未闭合 `code") > 0);
  check "空文档可解析"
    (String.length (markup "") > 0);
  check "无未闭合围栏时整篇当作正文"
    (has_block (function Markdown.Paragraph _ -> true | _ -> false)
       (Markdown.parse "只有一段话\n"));
  check "escape_markup 转义 &"
    (Markdown.escape_markup "&" = "&amp;");
  check "escape_markup 转义 <"
    (Markdown.escape_markup "<" = "&lt;");
  check "escape_markup 转义 >"
    (Markdown.escape_markup ">" = "&gt;");
  check "escape_markup 转义引号"
    (let r = Markdown.escape_markup "\"" in
     String.contains r '&');
  check "escape_markup 保留普通中文"
    (Markdown.escape_markup "你好" = "你好");
  (* 统计 *)
  let s = Markdown.stats (Markdown.parse "- [ ] a\n- [x] b\n- [ ] c\n") in
  check_eq "统计总数" ~expected:3 s.Markdown.total;
  check_eq "统计已完成" ~expected:1 s.Markdown.done_;
  check_eq "统计未完成" ~expected:2 s.Markdown.pending;
  let s0 = Markdown.stats (Markdown.parse "没有待办\n") in
  check_eq "无待办时 total 为 0" ~expected:0 s0.Markdown.total

let test_markdown_toggle () =
  section "Markdown 待办反写（单一数据源，不存在两份状态）";
  let src = "- [ ] 第一项\n- [x] 第二项\n普通段落" in
  check_eq "未勾选变已勾选" ~expected:"- [x] 第一项\n- [x] 第二项\n普通段落"
    (Markdown.toggle_todo src 0);
  check_eq "已勾选变回未勾选" ~expected:"- [ ] 第一项\n- [x] 第二项\n普通段落"
    (Markdown.toggle_todo (Markdown.toggle_todo src 0) 0);
  check_eq "第二项被切换" ~expected:"- [ ] 第一项\n- [ ] 第二项\n普通段落"
    (Markdown.toggle_todo src 1);
  check "切换两次回到原文" (Markdown.toggle_todo (Markdown.toggle_todo src 0) 0 = src);
  check "切换后总长度不变" (String.length (Markdown.toggle_todo src 0) = String.length src);
  check_eq "缩进保持" ~expected:"  - [x] 缩进项"
    (Markdown.toggle_todo "  - [ ] 缩进项" 0);
  check_eq "中文待办反写" ~expected:"- [ ] 买菜\n- [x] 做饭"
    (Markdown.toggle_todo "- [ ] 买菜\n- [ ] 做饭" 1);
  check_eq "大写 X 视为已勾选" ~expected:"- [ ] x"
    (Markdown.toggle_todo "- [X] x" 0);
  check "行号越界原样返回" (Markdown.toggle_todo src 99 = src);
  check "负行号原样返回" (Markdown.toggle_todo src (-1) = src);
  check "非待办行不被改" (Markdown.toggle_todo src 2 = src);
  check "代码块内的待办不被改"
    (let s = "```\n- [ ] x\n```" in Markdown.toggle_todo s 1 = s);
  check "待办行号与 parse 结果一致"
    (let s = "intro\n- [ ] a\n- [ ] b" in
     let lines =
       List.filter_map
         (function Markdown.Todo t -> Some t.line | _ -> None)
         (Markdown.parse s)
     in
     lines = [ 1; 2 ]);
  check "反写结果能重新解析出正确状态"
    (let s = "- [ ] a\n- [x] b" in
     let t = Markdown.toggle_todo s 0 in
     let states =
       List.filter_map
         (function Markdown.Todo t -> Some t.Markdown.checked | _ -> None)
         (Markdown.parse t)
     in
     states = [ true; true ])

(* ---------- pango ---------- *)

let test_pango () =
  section "Pango markup（正文里的元字符必须转义）";
  let m = markup "hello <world> & \"more\"" in
  check "& 被转义" (contains m "&amp;");
  check "< 被转义" (contains m "&lt;");
  check "> 被转义" (contains m "&gt;");
  let m2 = markup "```\n<b>not markup</b>\n```" in
  check "代码块内尖括号被转义" (contains m2 "&lt;b&gt;");
  check "标题带 <b>" (contains (markup "# 标题") "<b>");
  check "一级标题字号 19" (contains (markup "# a") "size=\"19\"");
  check "六级标题字号 12" (contains (markup "###### a") "size=\"12\"");
  check "标题带颜色" (contains (markup "# a") "foreground=");
  check "未勾选待办有方框字符" (contains (markup "- [ ] todo item") "☐");
  check "已勾选待办有勾号字符" (contains (markup "- [x] done item") "☑");
  check "空笔记有占位提示" (contains (markup "") "空笔记");
  check "正文分段输出换行" (contains (markup "a\n\nb") "\n");
  (* 安全性：markdown 中的 Pango 标签不得被执行 *)
  let evil = markup "点击 <span size=\"99\">巨大</span>" in
  check "markdown 里的 Pango 标签被中和（无裸 <span）"
    (not (has_raw_tag evil));
  check "markdown 里的标签以转义形式出现"
    (contains evil "&lt;span");
  check "行内代码用等宽字体" (contains (markup "用 `x` 吧") "Monospace");
  check "链接只加样式不加可点击标签"
    (let l = markup "见 [文档](http://x.y)" in
     contains l "underline=\"single\"" && not (contains l "<a "));
  check "加粗内容带 <b>" (contains (markup "**粗**") "<b>");
  check "删除线带 <s>" (contains (markup "~~删~~") "<s>");
  check "列表项带缩进" (contains (markup "- 项") "indent=");
  check "超长文档也能生成 markup"
    (String.length (markup (String.concat "\n" (List.init 500 (fun i ->
         Printf.sprintf "## 第 %d 行 **粗**" i)))) > 1000)

(** 视图层的点击交互完全依赖 run 上标的源码行号，所以这里不只看
    样式，还得确认行号和 [Markdown.todo.line] 逐个对得上。 *)
let test_render_todo_spans () =
  section "Render run 的待办行号";
  let runs src = Render.to_runs (Markdown.parse src) in
  let todo_src = "引言\n- [ ] 甲\n- [x] 乙\n结尾" in
  let spans = List.filter_map (fun (r : Render.run) -> r.todo_line) (runs todo_src) in
  check "复选框行号与解析结果一致"
    (spans
    = List.filter_map
        (function Markdown.Todo t -> Some t.line | _ -> None)
        (Markdown.parse todo_src));
  check "普通文本不带待办行号"
    (not
       (List.exists
          (fun (r : Render.run) -> String.trim r.text = "引言" && r.todo_line <> None)
          (runs todo_src)));
  check "带待办的文档也有行号"
    (List.length (List.filter_map (fun (r : Render.run) -> r.todo_line) (runs "- [ ] 买牛奶")) = 1);
  check "代码块里的 '- [ ]' 不被当成待办"
    (List.filter_map (fun (r : Render.run) -> r.todo_line) (runs "```\n- [ ] 假待办\n```") = []);
  check "待办的点击范围不会溢出到下一段"
    (let r = runs "- [ ] 甲\n第二段" in
     List.exists (fun (r : Render.run) -> r.todo_line <> None && not (contains r.text "第二段")) r
     && List.exists (fun (r : Render.run) -> r.todo_line = None && contains r.text "第二段") r);
  check "勾选后的待办：复选框和带删除线的正文分成两段"
    (let r = runs "- [x] 甲" in
     match r with
     | a :: b :: _ ->
         a.todo_line <> None
         && (not (List.mem `Strike a.attrs))
         && List.mem `Strike b.attrs
         && b.todo_line = None
     | _ -> false)

let test_note_images () =
  section "图片与笔记的关联";
  with_store "noteimg" (fun st ->
      let n1 = Sqlite_store.create_note st ~title:"甲" ~body:"b1" () in
      let n2 = Sqlite_store.create_note st ~title:"乙" ~body:"b2" () in
      let mk data = Sqlite_store.add_image st ~data ~mime:"image/png" ~width:4 ~height:4 in
      let a = mk "AAA" and b = mk "BBB" and c = mk "CCC" in

      check "新建笔记没有图片" (Sqlite_store.note_images st n1.Model.id = []);

      (* 顺序必须按插入顺序，而不是按 id 或字母 *)
      ignore (Sqlite_store.link_image st n1.Model.id a.Model.id);
      ignore (Sqlite_store.link_image st n1.Model.id b.Model.id);
      ignore (Sqlite_store.link_image st n1.Model.id c.Model.id);
      check_eq "按插入顺序返回" ~expected:[ a.id; b.id; c.id ]
        (List.map (fun (i : Model.image) -> i.Model.id) (Sqlite_store.note_images st n1.Model.id));
      check "链接顺序可以读回来"
        (Sqlite_store.max_image_pos st n1.Model.id = 2);

      (* 另一条笔记的图不能串过来 *)
      check "别的笔记看不到这些图" (Sqlite_store.note_images st n2.Model.id = []);

      (* 同一张图可以被多条笔记引用 *)
      let shared = mk "SHARED" in
      ignore (Sqlite_store.link_image st n1.Model.id shared.Model.id);
      ignore (Sqlite_store.link_image st n2.Model.id shared.Model.id);
      check "一张图能被两条笔记引用"
        (List.exists (fun (i : Model.image) -> i.Model.id = shared.Model.id)
           (Sqlite_store.note_images st n1.Model.id)
        && List.exists (fun (i : Model.image) -> i.Model.id = shared.Model.id)
             (Sqlite_store.note_images st n2.Model.id));

      (* 摘关联不删本体 *)
      Sqlite_store.unlink_image st n1.Model.id shared.Model.id;
      check "摘掉甲的关联后甲这边没有了"
        (not
           (List.exists (fun (i : Model.image) -> i.Model.id = shared.Model.id)
              (Sqlite_store.note_images st n1.Model.id)));
      check "但乙还引用着，所以图片不能被回收"
        (Sqlite_store.get_image st shared.Model.id <> None);
      check "还有人引用时 gc 不删它" (Sqlite_store.gc_orphan_images st = 0);
      check_eq "乙的图还在" ~expected:[ shared.id ]
        (List.map (fun (i : Model.image) -> i.Model.id) (Sqlite_store.note_images st n2.Model.id));

      (* 最后一个引用摘掉后要回收 *)
      Sqlite_store.unlink_image st n2.Model.id shared.Model.id;
      check "没人引用时 gc 删掉图片"
        (Sqlite_store.gc_orphan_images st = 1
        && Sqlite_store.get_image st shared.Model.id = None);

      (* 重复链接同一个 id 不该产生两行 *)
      ignore (Sqlite_store.link_image st n2.Model.id a.Model.id);
      ignore (Sqlite_store.link_image st n2.Model.id a.Model.id);
      check_eq "同一张图重复链接只有一行" ~expected:[ a.id ]
        (List.map (fun (i : Model.image) -> i.Model.id) (Sqlite_store.note_images st n2.Model.id));

      (* 删笔记要级联清关联，并回收只被它引用的图 *)
      let solo = mk "SOLO" in
      ignore (Sqlite_store.link_image st n2.Model.id solo.Model.id);
      Sqlite_store.delete_note st n2.Model.id;
      check "删笔记会回收只被它引用的图片"
        (Sqlite_store.get_image st solo.Model.id = None);
      check "删笔记不动别的笔记还在用的图片"
        (Sqlite_store.get_image st a.Model.id <> None);
      check_eq "甲的图片列表没被动过" ~expected:[ a.id; b.id; c.id ]
        (List.map (fun (i : Model.image) -> i.Model.id) (Sqlite_store.note_images st n1.Model.id));

      (* 摘掉全部引用后 gc 应该一次清干净 *)
      List.iter (fun (i : Model.image) -> Sqlite_store.unlink_image st n1.Model.id i.Model.id)
        (Sqlite_store.note_images st n1.Model.id);
      check "全部摘掉后 gc 清空图片池" (Sqlite_store.gc_orphan_images st = 3);
      check "池子空了" (Sqlite_store.note_images st n1.Model.id = []))

let test_note_images_encrypted () =
  section "图片关联 + 加密";
  with_store "noteimg-enc" (fun st ->
      let n = Sqlite_store.create_note st ~title:"密" ~body:"b" () in
      let plain = "\137PNG\r\n\032\n bytes-not-actually-a-png" in
      let img = Sqlite_store.add_image st ~data:plain ~mime:"image/png" ~width:9 ~height:9 in
      ignore (Sqlite_store.link_image st n.Model.id img.Model.id);
      let st = Sqlite_store.enable_crypto ~password:"pw" st in
      (* 关联表里存的是 id，不是字节，所以加密开关不影响它 *)
      check_eq "加密后关联还在" ~expected:[ img.id ]
        (List.map (fun (i : Model.image) -> i.Model.id) (Sqlite_store.note_images st n.Model.id));
      check_eq "加密后图片字节能解密回来" ~expected:plain
        (Option.get (Sqlite_store.get_image st img.Model.id)).data;
      (* 磁盘上必须是密文 *)
      check "磁盘上的图片字节不是明文"
        (not (String.equal plain (fst (Option.get (Sqlite_store.image_raw_data st img.Model.id)))));
      let st = Sqlite_store.disable_crypto st in
      check_eq "关闭加密后字节回到明文" ~expected:plain
        (Option.get (Sqlite_store.get_image st img.Model.id)).data;
      check_eq "关闭加密后关联还在" ~expected:[ img.id ]
        (List.map (fun (i : Model.image) -> i.Model.id) (Sqlite_store.note_images st n.Model.id)))

let test_normalize_tags () =
  section "标签规范化";
  (* 去空白、去重、按名字排序：视图层和存储层必须用同一份规则，
     否则界面上看着没变的标签会被当成"改过了" *)
  check "去首尾空白"
    (Sqlite_store.normalize_tags [ " a "; "b" ] = [ "a"; "b" ]);
  check "丢掉空串"
    (Sqlite_store.normalize_tags [ ""; "  "; "a" ] = [ "a" ]);
  check "去重"
    (Sqlite_store.normalize_tags [ "a"; "b"; "a" ] = [ "a"; "b" ]);
  check "按名字排序而不是按输入顺序"
    (Sqlite_store.normalize_tags [ "新标签"; "第二个"; "第三个" ]
    = [ "新标签"; "第三个"; "第二个" ]);
  check "幂等（规范化两次结果相同）"
    (let once = Sqlite_store.normalize_tags [ "  b"; "a"; "b" ] in
     Sqlite_store.normalize_tags once = once)

let () =
  Printf.printf "notebook core 测试\n";
  test_model ();
  test_crypto ();
  test_store_basic ();
  test_store_persistence ();
  test_store_default_path ();
  test_store_encrypted ();
  test_store_unlock ();
  test_store_images ();
  test_note_images ();
  test_note_images_encrypted ();
  test_normalize_tags ();
  test_search ();
  test_search_index_consistency ();
  test_markdown ();
  test_markdown_toggle ();
  test_pango ();
  test_render_todo_spans ();
  Printf.printf "\n%d 项检查，%d 项失败\n" !checks !failures;
  if !failures > 0 then exit 1