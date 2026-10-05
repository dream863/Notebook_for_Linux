(** SQLite 存储层：schema、迁移、CRUD、全文索引。

    这是 core 层里唯一碰 Sqlite3 的文件。它不知道 GUI 的存在，
    也不做 Markdown 解析 —— 只负责忠实地存取 [Model.t]。 *)

type t = {
  db : Sqlite3.db;
  (* None 表示未加密模式；Some ctx 表示正文的密文+nonce 分列存储 *)
  crypto : Crypto.t option;
}

exception Db_error of string

let failf fmt = Printf.ksprintf (fun s -> raise (Db_error s)) fmt

(* ---------- Sqlite3 的小工具 ---------- *)

let check = function
  | rc ->
      if not (Sqlite3.Rc.is_success rc) then
        failf "sqlite: %s" (Sqlite3.Rc.to_string rc)

let exec db sql = check (Sqlite3.exec db sql)

let query db sql f =
  let st = Sqlite3.prepare db sql in
  let rec go () =
    match Sqlite3.step st with
    | Sqlite3.Rc.ROW ->
        f st;
        go ()
    | Sqlite3.Rc.DONE -> ()
    | rc ->
        let e = Sqlite3.Rc.to_string rc in
        ignore (Sqlite3.finalize st);
        failf "sqlite step: %s" e
  in
  try
    go ();
    ignore (Sqlite3.finalize st)
  with e ->
    ignore (Sqlite3.finalize st);
    raise e

let exec_params_list db sql params f =
  let st = Sqlite3.prepare db sql in
  List.iteri (fun i p -> ignore (Sqlite3.bind st (i + 1) p)) params;
  let rec go () =
    match Sqlite3.step st with
    | Sqlite3.Rc.ROW ->
        f st;
        go ()
    | Sqlite3.Rc.DONE -> ()
    | rc ->
        let e = Sqlite3.Rc.to_string rc in
        ignore (Sqlite3.finalize st);
        failf "sqlite step: %s" e
  in
  try
    go ();
    ignore (Sqlite3.finalize st)
  with e ->
    ignore (Sqlite3.finalize st);
    raise e

let exec_params db sql f = exec_params_list db sql [] f

let exec_one_params db sql params f =
  let st = Sqlite3.prepare db sql in
  List.iteri (fun i p -> ignore (Sqlite3.bind st (i + 1) p)) params;
  match Sqlite3.step st with
  | Sqlite3.Rc.ROW ->
      let v = f st in
      ignore (Sqlite3.finalize st);
      Some v
  | Sqlite3.Rc.DONE ->
      ignore (Sqlite3.finalize st);
      None
  | rc ->
      ignore (Sqlite3.finalize st);
      failf "sqlite step: %s" (Sqlite3.Rc.to_string rc)

let exec_one db sql f = exec_one_params db sql [] f



let run db sql =
  let st = Sqlite3.prepare db sql in
  ignore (Sqlite3.step st);
  ignore (Sqlite3.finalize st)

let run_params db sql params =
  let st = Sqlite3.prepare db sql in
  List.iteri (fun i p -> ignore (Sqlite3.bind st (i + 1) p)) params;
  let rc = Sqlite3.step st in
  ignore (Sqlite3.finalize st);
  check rc

(* ---------- schema 与迁移 ---------- *)

(* 正文以密文形式落库：cipher 存密文，nonce 单独一列。
   未加密模式下 cipher 存原文、nonce 存空串，读取时靠 crypto 分支决定。 *)
(* crypto_meta 记录加密开关与 KDF 参数，version 用于将来的迁移。
   注意下面的 `{|...|}` 是字符串字面量，OCaml 注释不能写在里面 ——
   那样会被原样喂给 SQLite 当 SQL 报语法错。 *)
let schema =
  {|
CREATE TABLE IF NOT EXISTS note (
  id         INTEGER PRIMARY KEY AUTOINCREMENT,
  title      TEXT    NOT NULL DEFAULT '',
  cipher     BLOB,
  nonce      BLOB    NOT NULL DEFAULT x'',
  pinned     INTEGER NOT NULL DEFAULT 0,
  created_at REAL    NOT NULL,
  updated_at REAL    NOT NULL
);

CREATE TABLE IF NOT EXISTS tag (
  id   INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS note_tag (
  note_id INTEGER NOT NULL REFERENCES note(id) ON DELETE CASCADE,
  tag_id  INTEGER NOT NULL REFERENCES tag(id)  ON DELETE CASCADE,
  PRIMARY KEY (note_id, tag_id)
);

CREATE TABLE IF NOT EXISTS image (
  id         INTEGER PRIMARY KEY AUTOINCREMENT,
  data       BLOB    NOT NULL,
  nonce      BLOB    NOT NULL,
  mime       TEXT    NOT NULL,
  width      INTEGER NOT NULL DEFAULT 0,
  height     INTEGER NOT NULL DEFAULT 0,
  created_at REAL    NOT NULL
);

-- 图片存在独立的池子里，靠 note_image 挂到笔记上。
-- pos 保留插入顺序：同一个笔记里的图片是有先后的，
-- 只按 id 排序的话重开一次程序顺序就变了。
CREATE TABLE IF NOT EXISTS note_image (
  note_id  INTEGER NOT NULL REFERENCES note(id)  ON DELETE CASCADE,
  image_id INTEGER NOT NULL REFERENCES image(id) ON DELETE CASCADE,
  pos      INTEGER NOT NULL,
  PRIMARY KEY (note_id, image_id)
);

-- 回收孤儿图片要按 image_id 反查有没有笔记引用，没有索引就是全表扫
CREATE INDEX IF NOT EXISTS note_image_by_image ON note_image(image_id);

CREATE TABLE IF NOT EXISTS crypto_meta (
  id         INTEGER PRIMARY KEY CHECK (id = 1),
  encrypted  INTEGER NOT NULL,
  algorithm  TEXT    NOT NULL DEFAULT '',
  iterations INTEGER NOT NULL DEFAULT 0,
  salt       BLOB,
  verifier         BLOB,
  verifier_nonce   BLOB,
  version    INTEGER NOT NULL DEFAULT 1
);
|}

(* trigram 分词器是为了中文：unicode61 不切中文，
   而 trigram 要求查询词 >= 3 字符，所以 Search 模块对短词降级到 LIKE。 *)
let fts_schema =
  {|
CREATE VIRTUAL TABLE IF NOT EXISTS note_fts USING fts5(
  title,
  tokenize='trigram'
);
|}

(* 用触发器保持索引同步。这里只索引标题：正文加密后无法进 FTS，
   所以正文搜索走内存解密扫描（见 body_matches）。 *)
let fts_triggers =
  {|
CREATE TRIGGER IF NOT EXISTS note_ai AFTER INSERT ON note BEGIN
  INSERT INTO note_fts(rowid, title) VALUES (new.id, new.title);
END;
CREATE TRIGGER IF NOT EXISTS note_ad AFTER DELETE ON note BEGIN
  DELETE FROM note_fts WHERE rowid = old.id;
END;
CREATE TRIGGER IF NOT EXISTS note_au AFTER UPDATE ON note BEGIN
  DELETE FROM note_fts WHERE rowid = old.id;
  INSERT INTO note_fts(rowid, title) VALUES (new.id, new.title);
END;
|}

let migrate db =
  exec db "PRAGMA foreign_keys = ON";
  exec db "PRAGMA journal_mode = WAL";
  exec db schema;
  exec db fts_schema;
  exec db fts_triggers;
  (* 老库补 verifier 列：SQLite 的 CREATE TABLE IF NOT EXISTS 不会加列 *)
  List.iter
    (fun col ->
      let n = Sqlite3.prepare db "SELECT COUNT(*) FROM pragma_table_info('crypto_meta') WHERE name = ?1" in
      ignore (Sqlite3.bind_text n 1 col);
      let present =
        match Sqlite3.step n with
        | Sqlite3.Rc.ROW -> Sqlite3.column_int n 0 > 0
        | _ -> false
      in
      ignore (Sqlite3.finalize n);
      if not present then
        run db (Printf.sprintf "ALTER TABLE crypto_meta ADD COLUMN %s BLOB" col))
    [ "verifier"; "verifier_nonce" ];
  (* 已有笔记但 FTS 因异常错过的，补一次索引 *)
  exec db "INSERT INTO note_fts(note_fts) VALUES('rebuild')" |> ignore

let ensure_crypto_row db =
  match
    exec_one db "SELECT id FROM crypto_meta WHERE id = 1" (fun st ->
        ignore (Sqlite3.column_int st 0);
        true)
  with
  | Some _ -> ()
  | None ->
      run db
        "INSERT INTO crypto_meta(id, encrypted, algorithm, iterations, salt, version)
         VALUES (1, 0, '', 0, NULL, 1)"

let note_id_list t sql params =
  let ids = ref [] in
  let st = Sqlite3.prepare t.db sql in
  List.iter (fun (i, v) -> ignore (Sqlite3.bind st i v)) params;
  let rec go () =
    match Sqlite3.step st with
    | Sqlite3.Rc.ROW ->
        ids := Sqlite3.column_int st 0 :: !ids;
        go ()
    | Sqlite3.Rc.DONE -> ()
    | rc ->
        let e = Sqlite3.Rc.to_string rc in
        ignore (Sqlite3.finalize st);
        failf "search: %s" e
  in
  (try go (); ignore (Sqlite3.finalize st)
   with e ->
     ignore (Sqlite3.finalize st);
     raise e);
  List.rev !ids

(* FTS5 的 MATCH 语法里引号和运算符要转义，否则用户搜个 a* 就报错 *)

(** 直接取落盘的 (密文, nonce)，绕过解码路径。

    加密开关转换、测试校验都需要它；应用层不应调用。 *)
let raw_cipher_and_nonce t id =
  exec_one_params t.db
    "SELECT cipher, nonce FROM note WHERE id = ?1"
    [ Sqlite3.Data.INT (Int64.of_int id) ]
    (fun s -> (Sqlite3.column_blob s 0, Sqlite3.column_blob s 1))

let image_raw_data t id =
  exec_one_params t.db
    "SELECT data, nonce FROM image WHERE id = ?1"
    [ Sqlite3.Data.INT (Int64.of_int id) ]
    (fun s -> (Sqlite3.column_blob s 0, Sqlite3.column_blob s 1))

(* ---------- 打开 ---------- *)

let data_dir_default () =
  let base =
    match Sys.getenv_opt "XDG_DATA_HOME" with
    | Some d when d <> "" -> d
    | _ -> Filename.concat (Sys.getenv "HOME") ".local/share"
  in
  Filename.concat base "notebook"

let default_db_path () =
  match Sys.getenv_opt "NOTEBOOK_DB" with
  | Some p when p <> "" -> p
  | _ -> Filename.concat (data_dir_default ()) "notebook.db"

let rec mkdir_p dir =
  if dir <> "" && dir <> "/" && not (Sys.file_exists dir) then (
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let open_ ?path () =
  let path = match path with Some p -> p | None -> default_db_path () in
  mkdir_p (Filename.dirname path);
  let db = Sqlite3.db_open path in
  migrate db;
  ensure_crypto_row db;
  { db; crypto = None }

let close t = ignore (Sqlite3.db_close t.db)

(* ---------- 加密状态 ---------- *)

let is_encrypted t =
  match
    exec_one t.db "SELECT encrypted FROM crypto_meta WHERE id = 1" (fun st ->
        Sqlite3.column_int st 0 <> 0)
  with
  | Some b -> b
  | None -> false

let kdf_params t =
  exec_one t.db "SELECT algorithm, iterations, salt FROM crypto_meta WHERE id = 1" (fun st ->
      let salt =
        if Sqlite3.column_is_null st 2 then None
        else Some (Sqlite3.column_blob st 2)
      in
      (Sqlite3.column_text st 0, Sqlite3.column_int st 1, salt))
  |> Option.value ~default: ("", 0, None)

(* verifier 是用主密钥加密的一段固定文本。解锁时解密它来验证口令：
   比"拿第一条笔记试解"可靠 —— 笔记为空时后者无从判断。 *)
let verifier_plain = "notebook-ok"

let write_verifier db key =
  let cipher, nonce = Crypto.encrypt key verifier_plain in
  run_params db
    "UPDATE crypto_meta SET verifier = ?1, verifier_nonce = ?2 WHERE id = 1"
    [ Sqlite3.Data.BLOB cipher; Sqlite3.Data.BLOB nonce ]

let stored_verifier db =
  exec_one db "SELECT verifier, verifier_nonce FROM crypto_meta WHERE id = 1" (fun st ->
      if Sqlite3.column_is_null st 0 then None
      else Some (Sqlite3.column_blob st 0, Sqlite3.column_blob st 1))
  |> Option.value ~default:None

(** 首次设置口令：生成盐、派生密钥、把库里已有的明文正文和图片全部转成密文。 *)
let enable_crypto ~password t =
  if t.crypto <> None then failf "数据库已处于加密状态";
  let salt = Crypto.new_salt () in
  let key = Crypto.create ~password ~salt in
  (* 先把明文数据全部加密，再翻开关；中途失败会留下半明文半密文的
     数据库，所以整个过程放在一个事务里 *)
  exec t.db "BEGIN IMMEDIATE";
  match
    (try
       let ids = note_id_list t "SELECT id FROM note" [] in
       List.iter
         (fun id ->
           let cipher, nonce =
             match raw_cipher_and_nonce t id with
             | Some (c, "") -> Crypto.encrypt key c
             | Some (c, n) -> (c, n)
             | None -> ("", "")
           in
           run_params t.db "UPDATE note SET cipher = ?1, nonce = ?2 WHERE id = ?3"
             [ Sqlite3.Data.BLOB cipher; Sqlite3.Data.BLOB nonce;
               Sqlite3.Data.INT (Int64.of_int id) ])
         ids;
       let img_ids = note_id_list t "SELECT id FROM image" [] in
       List.iter
         (fun id ->
           match image_raw_data t id with
           | Some (c, "") -> (
               let cipher, nonce = Crypto.encrypt key c in
               run_params t.db "UPDATE image SET data = ?1, nonce = ?2 WHERE id = ?3"
                 [ Sqlite3.Data.BLOB cipher; Sqlite3.Data.BLOB nonce;
                   Sqlite3.Data.INT (Int64.of_int id) ])
           | Some _ | None -> ())
         img_ids;
       run_params t.db
         "UPDATE crypto_meta SET encrypted = 1, algorithm = ?1, iterations = ?2,
                 salt = ?3, version = 1 WHERE id = 1"
         [ Sqlite3.Data.TEXT Crypto.kdf_algorithm;
           Sqlite3.Data.INT (Int64.of_int Crypto.kdf_iterations);
           Sqlite3.Data.BLOB salt ];
       write_verifier t.db key;
       Ok key
     with e -> Error e)
  with
  | Ok key ->
      exec t.db "COMMIT";
      { t with crypto = Some key }
  | Error e ->
      ignore (Sqlite3.exec t.db "ROLLBACK");
      Crypto.wipe key;
      raise e

(** 关闭口令：把库里所有密文解密回明文。 *)
let disable_crypto t =
  match t.crypto with
  | None -> failf "数据库当前不是加密状态"
  | Some key ->
      exec t.db "BEGIN IMMEDIATE";
      (match
         (try
            let ids = note_id_list t "SELECT id FROM note" [] in
            List.iter
              (fun id ->
                match raw_cipher_and_nonce t id with
                | Some (c, n) when n <> "" ->
                    let plain = Crypto.decrypt key ~cipher:c ~nonce:n in
                    run_params t.db
                      "UPDATE note SET cipher = ?1, nonce = x'' WHERE id = ?2"
                      [ Sqlite3.Data.TEXT plain;
                        Sqlite3.Data.INT (Int64.of_int id) ]
                | _ -> ())
              ids;
            let img_ids = note_id_list t "SELECT id FROM image" [] in
            List.iter
              (fun id ->
                match image_raw_data t id with
                | Some (c, n) when n <> "" ->
                    let plain = Crypto.decrypt key ~cipher:c ~nonce:n in
                    run_params t.db
                      "UPDATE image SET data = ?1, nonce = x'' WHERE id = ?2"
                      [ Sqlite3.Data.TEXT plain;
                        Sqlite3.Data.INT (Int64.of_int id) ]
                | _ -> ())
              img_ids;
            run_params t.db
              "UPDATE crypto_meta SET encrypted = 0, algorithm = '', iterations = 0,
                      salt = NULL, verifier = NULL, verifier_nonce = NULL
               WHERE id = 1"
              [];
            Ok ()
          with e -> Error e)
       with
       | Ok () ->
           exec t.db "COMMIT";
           Crypto.wipe key;
           { t with crypto = None }
       | Error e ->
           ignore (Sqlite3.exec t.db "ROLLBACK");
           raise e)

(** 解锁：验证口令，正确则返回带密钥的 store。

    口令错误抛 [Db_error "口令错误"]，而不是让上层拿到一堆乱码。 *)
let unlock ~password t =
  if not (is_encrypted t) then failf "数据库未加密，无需解锁";
  match t.crypto with
  | Some _ -> t
  | None ->
      let _, _, salt = kdf_params t in
      (match salt with
       | None -> failf "数据库标记为加密但缺少盐，无法解锁"
       | Some salt ->
           let key = Crypto.create ~password ~salt in
           let ok =
             match stored_verifier t.db with
             | None -> false
             | Some (c, n) -> (
                 match Crypto.decrypt key ~cipher:c ~nonce:n with
                 | v -> v = verifier_plain
                 | exception Crypto.Crypto_exn Crypto.Bad_password -> false)
           in
           if ok then { t with crypto = Some key }
           else begin
             Crypto.wipe key;
             failf "口令错误"
           end)

(* ---------- 正文编解码 ---------- *)

let decode_body t cipher nonce =
  match t.crypto with
  | Some key -> Crypto.decrypt key ~cipher ~nonce
  | None ->
      if is_encrypted t then failf "数据库已加密，请先解锁再读取"
      else cipher

let encode_body t body =
  match t.crypto with
  | None -> (body, "")
  | Some key -> Crypto.encrypt key body

(* ---------- 标签 ---------- *)

let tags_of_note db note_id =
  let acc = ref [] in
  exec_params_list db
    "SELECT tg.name FROM tag tg JOIN note_tag nt ON nt.tag_id = tg.id
     WHERE nt.note_id = ?1 ORDER BY tg.name"
    [ Sqlite3.Data.INT (Int64.of_int note_id) ] (fun st ->
      acc := Sqlite3.column_text st 0 :: !acc);
  List.rev !acc

(** 标签的规范形式：去空白、去重、按名字排序。

    视图层的标签输入框也走这个函数：否则界面里显示的是"用户敲的顺序"、
    库里存的是"排序后的顺序"，两边看着不一样，每次编辑都会被当成改动。 *)
let normalize_tags tags =
  tags |> List.map String.trim |> List.filter (fun s -> s <> "") |> List.sort_uniq String.compare

let set_tags t note_id tags =
  run_params t.db "DELETE FROM note_tag WHERE note_id = ?1" [ Sqlite3.Data.INT (Int64.of_int note_id) ];
  List.iter
    (fun name ->
      run_params t.db "INSERT OR IGNORE INTO tag(name) VALUES (?1)" [ Sqlite3.Data.TEXT name ];
      let tag_id =
        exec_one_params t.db "SELECT id FROM tag WHERE name = ?1" [ Sqlite3.Data.TEXT name ]
          (fun st -> Sqlite3.column_int st 0)
        |> Option.value ~default:0
      in
      if tag_id > 0 then
        run_params t.db
          "INSERT OR IGNORE INTO note_tag(note_id, tag_id) VALUES (?1, ?2)"
          [ Sqlite3.Data.INT (Int64.of_int note_id); Sqlite3.Data.INT (Int64.of_int tag_id) ])
    (normalize_tags tags)

let all_tags t =
  let acc = ref [] in
  exec_params t.db "SELECT name FROM tag ORDER BY name" (fun st ->
      acc := Sqlite3.column_text st 0 :: !acc);
  List.rev !acc

(* ---------- 笔记 CRUD ---------- *)

let row_to_note t st =
  let id = Sqlite3.column_int st 0 in
  let cipher =
    if Sqlite3.column_is_null st 1 then "" else Sqlite3.column_blob st 1
  in
  let nonce =
    if Sqlite3.column_is_null st 2 then "" else Sqlite3.column_blob st 2
  in
  let body =
    try decode_body t cipher nonce
    with Crypto.Crypto_exn Crypto.Bad_password ->
      failf "笔记 %d 解密失败：口令错误或数据损坏" id
  in
  Model.make ~id ~title:(Sqlite3.column_text st 3) ~body
    ~tags:(tags_of_note t.db id)
    ~pinned:(Sqlite3.column_int st 4 <> 0)
    ~created_at:(Sqlite3.column_double st 5)
    ~updated_at:(Sqlite3.column_double st 6)

let select_columns = "id, cipher, nonce, title, pinned, created_at, updated_at"

let create_note t ?(title = "") ?(body = "") ?(tags = []) ?(pinned = false) () =
  let now = Unix.gettimeofday () in
  let cipher, nonce = encode_body t body in
  run_params t.db
    "INSERT INTO note(title, cipher, nonce, pinned, created_at, updated_at)
     VALUES (?1, ?2, ?3, ?4, ?5, ?6)"
    [
      Sqlite3.Data.TEXT title;
      Sqlite3.Data.BLOB cipher;
      Sqlite3.Data.BLOB nonce;
      Sqlite3.Data.INT (Int64.of_int (if pinned then 1 else 0));
      Sqlite3.Data.FLOAT now;
      Sqlite3.Data.FLOAT now;
    ];
  let id = Int64.to_int (Sqlite3.last_insert_rowid t.db) in
  set_tags t id tags;
  Model.make ~id ~title ~body ~tags ~pinned ~created_at:now ~updated_at:now

let save_note t n =
  let cipher, nonce = encode_body t n.Model.body in
  run_params t.db
    "UPDATE note SET title = ?1, cipher = ?2, nonce = ?3, pinned = ?4,
            updated_at = ?5 WHERE id = ?6"
    [
      Sqlite3.Data.TEXT n.title;
      Sqlite3.Data.BLOB cipher;
      Sqlite3.Data.BLOB nonce;
      Sqlite3.Data.INT (Int64.of_int (if n.pinned then 1 else 0));
      Sqlite3.Data.FLOAT n.updated_at;
      Sqlite3.Data.INT (Int64.of_int n.id);
    ];
  set_tags t n.id n.tags

let get_note t id =
  exec_one_params t.db
    (Printf.sprintf "SELECT %s FROM note WHERE id = ?1" select_columns)
    [ Sqlite3.Data.INT (Int64.of_int id) ]
    (row_to_note t)

let list_notes t =
  let acc = ref [] in
  exec_params t.db
    (Printf.sprintf "SELECT %s FROM note ORDER BY pinned DESC, updated_at DESC"
       select_columns)
    (fun st -> acc := row_to_note t st :: !acc);
  List.rev !acc

let set_pinned t id pinned =
  run_params t.db
    "UPDATE note SET pinned = ?1, updated_at = ?2 WHERE id = ?3"
    [
      Sqlite3.Data.INT (Int64.of_int (if pinned then 1 else 0));
      Sqlite3.Data.FLOAT (Unix.gettimeofday ());
      Sqlite3.Data.INT (Int64.of_int id);
    ]

(** 直接取落盘的 (密文, nonce)，绕过解码路径。

    测试和"确认磁盘上确实是密文"这类场景要用它；
    应用层不应调用。 *)
(* ---------- 图片 ---------- *)

let add_image t ~data ~mime ~width ~height =
  let cipher, nonce =
    match t.crypto with
    | None -> (data, "")
    | Some key -> Crypto.encrypt key data
  in
  run_params t.db
    "INSERT INTO image(data, nonce, mime, width, height, created_at)
     VALUES (?1, ?2, ?3, ?4, ?5, ?6)"
    [
      Sqlite3.Data.BLOB cipher;
      Sqlite3.Data.BLOB nonce;
      Sqlite3.Data.TEXT mime;
      Sqlite3.Data.INT (Int64.of_int width);
      Sqlite3.Data.INT (Int64.of_int height);
      Sqlite3.Data.FLOAT (Unix.gettimeofday ());
    ];
  let id = Int64.to_int (Sqlite3.last_insert_rowid t.db) in
  { Model.id; mime; width; height }

let get_image t id =
  exec_one_params t.db
    "SELECT data, nonce, mime, width, height FROM image WHERE id = ?1"
    [ Sqlite3.Data.INT (Int64.of_int id) ]
    (fun st ->
      let cipher = Sqlite3.column_blob st 0 in
      let nonce = Sqlite3.column_blob st 1 in
      let data =
        match t.crypto with
        | None -> cipher
        | Some key -> Crypto.decrypt key ~cipher ~nonce
      in
      let meta : Model.image =
        {
          Model.id;
          mime = Sqlite3.column_text st 2;
          width = Sqlite3.column_int st 3;
          height = Sqlite3.column_int st 4;
        }
      in
      let out : Model.image_data = { image = meta; data } in
      out)

let delete_image t id =
  run_params t.db "DELETE FROM image WHERE id = ?1" [ Sqlite3.Data.INT (Int64.of_int id) ]

(* ---------- 图片与笔记的关联 ---------
   图片字节放在共享池里，同一张图可以被多条笔记引用，所以删除要走
   两步：先摘关联，再看有没有人剩。直接 DELETE FROM image 会把别的
   笔记正在用的图一起删掉。 *)

let max_image_pos t note_id =
  match
    exec_one_params t.db "SELECT COALESCE(MAX(pos), -1) FROM note_image WHERE note_id = ?1"
      [ Sqlite3.Data.INT (Int64.of_int note_id) ] (fun st -> Sqlite3.column_int st 0)
  with
  | Some p -> p
  | None -> -1

(** 把图片挂到笔记末尾。 *)
let link_image t note_id image_id =
  let pos = max_image_pos t note_id + 1 in
  run_params t.db
    "INSERT OR REPLACE INTO note_image(note_id, image_id, pos) VALUES (?1, ?2, ?3)"
    [ Sqlite3.Data.INT (Int64.of_int note_id); Sqlite3.Data.INT (Int64.of_int image_id);
      Sqlite3.Data.INT (Int64.of_int pos) ];
  pos

(** 摘掉关联，但不碰图片本体。 *)
let unlink_image t note_id image_id =
  run_params t.db "DELETE FROM note_image WHERE note_id = ?1 AND image_id = ?2"
    [ Sqlite3.Data.INT (Int64.of_int note_id); Sqlite3.Data.INT (Int64.of_int image_id) ]

(** 一条笔记的图片，按插入顺序，**只含元数据**。

    刻意不在这里读 BLOB：刷新列表会把每条笔记都调一遍，把所有图片
    字节都解密读出来会让启动慢上几百毫秒到几秒，而列表上根本不用
    显示图。要字节的话单独调 [get_image]。 *)
let note_images t note_id =
  let acc = ref [] in
  exec_params_list t.db
    "SELECT i.id, i.mime, i.width, i.height
       FROM note_image ni JOIN image i ON i.id = ni.image_id
      WHERE ni.note_id = ?1
      ORDER BY ni.pos, i.id"
    [ Sqlite3.Data.INT (Int64.of_int note_id) ]
    (fun st ->
      let meta : Model.image =
        {
          Model.id = Sqlite3.column_int st 0;
          mime = Sqlite3.column_text st 1;
          width = Sqlite3.column_int st 2;
          height = Sqlite3.column_int st 3;
        }
      in
      acc := meta :: !acc);
  List.rev !acc

(** 删掉没有任何笔记引用的图片，返回删掉几张。

    图片可能很大（手机拍一张好几 MB），泄漏一张就白占一份空间，
    所以摘掉关联后立刻回收。 *)
let gc_orphan_images t =
  let before =
    match
      exec_one t.db "SELECT COUNT(*) FROM image" (fun st -> Sqlite3.column_int st 0)
    with
    | Some c -> c
    | None -> 0
  in
  run t.db
    "DELETE FROM image
      WHERE id NOT IN (SELECT image_id FROM note_image)";
  let after =
    match
      exec_one t.db "SELECT COUNT(*) FROM image" (fun st -> Sqlite3.column_int st 0)
    with
    | Some c -> c
    | None -> 0
  in
  before - after

(** 删笔记。正文、图片、标签、关联行都由外键 ON DELETE CASCADE 清掉
    （note_image 也在 CASCADE 里），这里只需删笔记本身，再回收它独占的图片。 *)
let delete_note t id =
  run_params t.db "DELETE FROM note WHERE id = ?1" [ Sqlite3.Data.INT (Int64.of_int id) ];
  run_params t.db "DELETE FROM note_tag WHERE note_id = ?1"
    [ Sqlite3.Data.INT (Int64.of_int id) ];
  ignore (gc_orphan_images t)


(* ---------- 搜索 ---------
   分级策略：
   - 查询词 >= 3 字符 -> FTS5 trigram，走索引
   - 查询词 1~2 字符（"备忘"、"地址" 这类高频短词）
     -> LIKE 全表扫描。个人笔记量级（<1万条）下依然是毫秒级。
   trigram 对 <3 字符直接 0 命中，这是实测行为，必须分流。 *)

let min_trigram_len = 3

(** UTF-8 字符数。不能用 [String.length]，那是字节数：
    "列表" 是 2 个字符但 6 字节，按字节判断会让短查询错走 trigram 路径。 *)
let char_length s =
  let n = ref 0 and i = ref 0 and len = String.length s in
  while !i < len do
    let c = Char.code s.[!i] in
    let step =
      if c < 0x80 then 1
      else if c land 0xE0 = 0xC0 then 2
      else if c land 0xF0 = 0xE0 then 3
      else if c land 0xF8 = 0xF0 then 4
      else 1
    in
    incr n;
    i := !i + step
  done;
  !n

let contains_substring needle haystack =
  let n = String.length needle and h = String.length haystack in
  if n = 0 then true
  else if n > h then false
  else begin
    let found = ref false and i = ref 0 in
    while (not !found) && !i <= h - n do
      if String.sub haystack !i n = needle then found := true else incr i
    done;
    !found
  end

let escape_fts s =
  let b = Buffer.create (String.length s + 8) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\"\""
      | '*' | '(' | ')' | ':' | '^' | '-' | '+' -> Buffer.add_char b ' '
      | _ -> Buffer.add_char b c)
    s;
  Buffer.contents b

let escape_like s =
  let b = Buffer.create (String.length s + 8) in
  String.iter
    (fun c ->
      match c with
      | '%' | '_' -> Buffer.add_char b '\\'
      | _ -> Buffer.add_char b c)
    s;
  Buffer.contents b

(** 标题命中（走 FTS5 索引）。 *)
let search_titles t query =
  let q = String.trim query in
  if q = "" then []
  else if char_length q < min_trigram_len then
    let like = "%" ^ escape_like q ^ "%" in
    note_id_list t
      "SELECT id FROM note WHERE title LIKE ?1 ESCAPE '\\' ORDER BY pinned DESC, updated_at DESC"
      [ (1, Sqlite3.Data.TEXT like) ]
  else
    note_id_list t
      "SELECT f.rowid FROM note_fts f WHERE f.note_fts MATCH ?1
       ORDER BY (SELECT pinned FROM note WHERE id = f.rowid) DESC,
                (SELECT updated_at FROM note WHERE id = f.rowid) DESC"
      [ (1, Sqlite3.Data.TEXT (escape_fts q)) ]

(** 正文命中：正文是密文，SQLite 侧无法建索引，只能在内存里解密后匹配。

    注意这是方案里"只加密正文"的直接后果：正文搜索无法走 FTS5 索引，
    只能扫全部已解密正文。个人笔记量级下开销可接受，换来的是正文密文存储。

    大小写不敏感：用 ASCII 小写比较；中文无大小写，天然正确。 *)
let body_matches ~query body =
  let q = String.trim query in
  if q = "" then false
  else
    contains_substring (String.lowercase_ascii q)
      (String.lowercase_ascii body)
