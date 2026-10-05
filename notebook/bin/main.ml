(** 入口。可选地传一个数据库路径：不传就用
    [Sqlite_store.default_db_path]（$NOTEBOOK_DB 或 XDG 下的默认位置）。

    一条命令行参数就够，不值得为它引 arg —— 这个程序没有别的子命令。 *)

let () =
  let path =
    match Array.to_list Sys.argv with
    | [ _; p ] when p <> "" -> Some p
    | _ -> None
  in
  (* 0 = 正常退出，1 = 口令放弃解锁。 *)
  exit (if Notebook_view.App.run ?path () then 0 else 1)
