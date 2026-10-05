(** Markdown 预览：用 Gtk TextTag 渲染 [Render.to_runs] 的结果。

    两个刻意的选择：

    1. **不经过 Pango markup 字符串。** GTK3 的 TextView 不解析 markup，
       lablgtk3 3.1.5 也没有 markup text view，所以富文本只能自己贴
       tag。而"生成 markup 再解析回来"比"直接从 AST 贴 tag"多一层
       不可信的中间表示 —— 正文里的 `<`/`&` 本来就不用参与解析。

    2. **待办勾选靠点击命中。** 预览整体只读，但待办的复选框是活的：
       core 在复选框 run 上标了源码行号（[Render.run.todo_line]），
       这里把渲染后的偏移区间记下来，点击时反查行号并回调上层，
       由上层调 [Markdown.toggle_todo] 改源码后重新渲染。 *)

(* ---------- 单位换算 ----------

   TextTag 的 `SIZE` 和 `INDENT` 都用 Pango 单位（1/1024 pt），
   而 core 的 attr 里 `Size n` 是点、`Indent n` 是 px。
   这一步漏掉的话字号会差 1024 倍，缩进会差得没法看。 *)
let pango_scale = 1024
let pt_of_px px = float_of_int px *. 0.75 (* 96 dpi：1px = 0.75pt *)
let px_to_pango px = int_of_float (pt_of_px px *. float_of_int pango_scale)
let pt_to_pango pt = pt * pango_scale

let tag_props_of_attr : Render.attr -> GText.tag_property list = function
  | `Bold -> [ `WEIGHT `BOLD ]
  | `Italic -> [ `STYLE `ITALIC ]
  | `Strike -> [ `STRIKETHROUGH true ]
  | `Underline -> [ `UNDERLINE `SINGLE ]
  | `Baseline -> [ `RISE (pt_to_pango 2) ]
  | `Size pt -> [ `SIZE (pt_to_pango pt) ]
  | `Color c -> [ `FOREGROUND c ]
  | `Indent px -> [ `INDENT (px_to_pango px) ]
  | `Mono -> [ `FAMILY "Monospace" ]
  | `Code -> [ `BACKGROUND "#f6f5f4" ]

type t = {
  view : GText.view;
  buffer : GText.buffer;
  (** 按 attr 组合缓存 tag。同一个组合在长文档里会出现几十次，
      不缓存就是几十个等价的 tag 对象，tag table 会迅速臃肿。 *)
  tags : (Render.attr list, GText.tag) Hashtbl.t;
  mutable todo_spans : (int * int * int) list;  (** (start, stop, 源码行) *)
  mutable on_todo_click : (int -> unit) option;
}

let view t = t.view

let tag_for t attrs =
  match Hashtbl.find_opt t.tags attrs with
  | Some tag -> tag
  | None ->
      let props = List.concat_map tag_props_of_attr attrs in
      let tag = t.buffer#create_tag props in
      Hashtbl.add t.tags attrs tag;
      tag

let offset_at_end t = (t.buffer#end_iter)#offset

let clear t =
  Hashtbl.reset t.tags;
  t.todo_spans <- [];
  let start = t.buffer#start_iter and stop = t.buffer#end_iter in
  t.buffer#delete ~start ~stop

let render_source t src =
  clear t;
  let runs = Render.to_runs (Markdown.parse src) in
  if Render.is_empty_runs runs then
    t.buffer#insert "（空笔记）"
  else
    List.iter
      (fun (r : Render.run) ->
        let start = offset_at_end t in
        let tags = if r.attrs = [] then [] else [ tag_for t r.attrs ] in
        t.buffer#insert ~tags r.text;
        let stop = offset_at_end t in
        (match r.todo_line with
         | Some line -> t.todo_spans <- (start, stop, line) :: t.todo_spans
         | None -> ()))
      runs;
    t.todo_spans <- List.rev t.todo_spans

(** 命中点击：返回被点到的待办源码行号。 *)
let todo_line_at_offset t offset =
  List.find_map
    (fun (start, stop, line) -> if offset >= start && offset <= stop then Some line else None)
    t.todo_spans

let set_on_todo_click t f = t.on_todo_click <- f

let create () =
  let buffer = GText.buffer () in
  let v = GText.view ~buffer ~editable:false ~cursor_visible:true ~wrap_mode:`WORD () in
  v#set_left_margin 14;
  v#set_right_margin 14;
  v#set_top_margin 10;
  v#set_bottom_margin 10;
  let t = { view = v; buffer; tags = Hashtbl.create 64; todo_spans = []; on_todo_click = None } in
  ignore
    (v#event#connect #button_press ~callback:(fun (ev : GdkEvent.Button.t) ->
         if GdkEvent.Button.button ev <> 1 then false
         else
           let x = int_of_float (GdkEvent.Button.x ev) in
           let y = int_of_float (GdkEvent.Button.y ev) in
           let offset = (v#get_iter_at_location ~x ~y)#offset in
           match (t.on_todo_click, todo_line_at_offset t offset) with
           | Some f, Some line ->
               f line;
               true (* 已消费：别让 TextView 同时去做选区 *)
           | _ -> false));
  t