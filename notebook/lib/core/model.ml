(** 领域模型：纯数据类型与不变量，不依赖 GUI，也不依赖存储。

    这一层刻意做成"不可变 + 显式"：所有更新都返回新值，
    这样上层（视图层）可以自由地做增量刷新而不需要 diff 库。 *)

(** 图片元数据：不含字节。

    字节存在 [Sqlite_store.get_image] 返回的 [image_data] 里，
    这样列表渲染图片引用时不必把整张图读进内存。 *)
type image = {
  id : int;
  mime : string;
  width : int;
  height : int;
}

(** 带字节的图片，仅在真正要显示/导出时构造。 *)
type image_data = {
  image : image;
  data : string;
}

type note = {
  id : int;
  title : string;
  body : string;  (** 加密开启时这里是明文，仅存在于内存中 *)
  tags : string list;
  pinned : bool;
  created_at : float;
  updated_at : float;
}

let make ~id ~title ~body ~tags ~pinned ~created_at ~updated_at =
  { id; title; body; tags; pinned; created_at; updated_at }

let with_body n body = { n with body }
let with_title n title = { n with title }
let with_tags n tags = { n with tags }
let with_pinned n pinned = { n with pinned }
let touch n = { n with updated_at = Unix.gettimeofday () }

(** 标题为空时用正文首行兜底，列表里不会出现空白行。 *)
let display_title n =
  let t = String.trim n.title in
  if t <> "" then t
  else
    let first =
      String.split_on_char '\n' n.body |> List.find_opt (fun l -> String.trim l <> "")
      |> Option.value ~default:"" |> String.trim
    in
    let stripped =
      let s = if String.length first > 60 then String.sub first 0 60 else first in
      s
    in
    if stripped = "" then "无标题" else stripped

(** 排序：置顶优先，其次按更新时间倒序。

    视图层直接依赖这个函数，保证"列表顺序"只有一处定义。 *)
let compare_for_list a b =
  match (a.pinned, b.pinned) with
  | true, false -> -1
  | false, true -> 1
  | _ -> Stdlib.compare b.updated_at a.updated_at

let sort_for_list notes = List.sort compare_for_list notes
