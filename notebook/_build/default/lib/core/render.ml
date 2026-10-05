(** Markdown AST -> 带样式的文本片段（Pango markup 或 Gtk TextTag）。

    模块名不叫 Pango：lablgtk3 自带一个 Pango 模块，在 view 层会
    把 core 这个遮掉，留到 view 阶段才发现就晚了。

    预览走 Pango 渲染，不走 WebKit：零额外 C 依赖、启动快、内存低。

    单一数据源是 [to_runs] —— 把 AST 拍平成 [text + 样式属性] 的片段序列，
    上层再翻译成各自的富文本表示：
    - [to_markup] 拼 Pango markup（tooltip、测试、导出）
    - view 层把这些 run 贴成 Gtk TextTag（预览正文）

    早先版本直接把 markdown 交给 Pango markup 渲染，有两个问题：
    1. 正文里的 `<` `&` 会破坏解析，必须转义（转义对了也就对了，
       但多一层字符串拼接和一层信任边界）；
    2. GTK3 的 TextView 不解析 markup —— lablgtk3 3.1.5 没有
       markup text view，想要富文本只能自己贴 TextTag。
    既然迟早要贴 tag，直接从 AST 生成 run 最省事。 *)

(** 样式属性。

    刻意只用我们自己这组原子属性，不直接暴露 [Pango.Tags] 或
    [GText.tag_property]：core 层不依赖 GTK（见 dune 里的库依赖），
    "AST 属性 -> TextTag 属性"的映射放在 view 层。 *)
type attr =
  [ `Bold
  | `Italic
  | `Strike
  | `Mono
  | `Code
  | `Size of int
  | `Color of string
  | `Indent of int
  | `Underline
  | `Baseline ]

(** 一段文本加样式。

    [todo_line] 是给视图层留的交互线索：非 [None] 时表示这段文本以
    该行的待办复选框开头，视图层点到这里就调 [Markdown.toggle_todo]
    反写源码。core 只负责"哪儿是复选框"，怎么响应由 view 层决定。 *)
type run = { text : string; attrs : attr list; todo_line : int option }

let size_of_heading = function
  | 1 -> 19
  | 2 -> 17
  | 3 -> 15
  | 4 -> 14
  | 5 -> 13
  | _ -> 12

let color_of_heading lvl =
  match lvl with
  | 1 -> "#1a5fb4"
  | 2 -> "#1c71d8"
  | 3 -> "#26a269"
  | 4 -> "#e5a50a"
  | _ -> "#77767b"

(** 追加一段文本，属性相同的相邻段会合并。

    合并时保留已有的 [todo_line]：待办复选框和它的正文文本属性完全相同
    （没勾选时），合并成一段是必然的，此时若丢掉行号，点击就再也命中
    不到了。反过来（新段带 todo 而旧段不带）不合并，因为那说明旧段是
    别的块，合并会把点击范围溢出到下一块。 *)
let push ?todo buf text attrs =
  if text <> "" then
    match !buf with
    | { text = t; attrs = a; todo_line = todo0 } :: rest
      when a = attrs && (todo0 <> None || Option.is_none todo) ->
        let todo_line = if todo0 <> None then todo0 else todo in
        buf := { text = t ^ text; attrs; todo_line } :: rest
    | _ -> buf := { text; attrs; todo_line = todo } :: !buf

let rec inline_runs buf attrs (items : Markdown.inline list) =
  match items with
  | [] -> ()
  | Markdown.Plain s :: rest ->
      push buf s attrs;
      inline_runs buf attrs rest
  | Markdown.Bold xs :: rest ->
      inline_runs buf (`Bold :: attrs) xs;
      inline_runs buf attrs rest
  | Markdown.Italic xs :: rest ->
      inline_runs buf (`Italic :: attrs) xs;
      inline_runs buf attrs rest
  | Markdown.Strike xs :: rest ->
      inline_runs buf (`Strike :: attrs) xs;
      inline_runs buf attrs rest
  | Markdown.Code s :: rest ->
      push buf s (`Code :: `Mono :: attrs);
      inline_runs buf attrs rest
  (* 预览态不做可点击链接：既能避免引入外链/导航面，
     也让 Pango 版本不引入 XSS 面。只给样式。 *)
  | Markdown.Link (_url, label) :: rest ->
      inline_runs buf (`Underline :: `Color "#1c71d8" :: attrs) label;
      inline_runs buf attrs rest

let block_runs buf = function
  | Markdown.Blank -> push buf "\n" []
  | Markdown.Rule -> push buf (String.make 28 '-') [ `Color "#c0c0c0" ]
  | Markdown.Heading (lvl, inl) ->
      push buf "\n" [];
      inline_runs buf [ `Bold; `Size (size_of_heading lvl); `Color (color_of_heading lvl) ] inl;
      push buf "\n" []
  | Markdown.Paragraph inl ->
      inline_runs buf [] inl;
      push buf "\n" []
  | Markdown.Quote inl ->
      inline_runs buf [ `Color "#77767b"; `Indent 12 ] inl;
      push buf "\n" []
  | Markdown.Bullet inl ->
      push buf "• " [ `Indent 16 ];
      inline_runs buf [] inl;
      push buf "\n" []
  | Markdown.Ordered (num, inl) ->
      push buf (Printf.sprintf "%d. " num) [ `Indent 16 ];
      inline_runs buf [] inl;
      push buf "\n" []
  | Markdown.Todo { line; checked; text; _ } ->
      (* 复选框本体只做视觉呈现，但带上源码行号：视图层命中它就把点击
         反写成源码（[Markdown.toggle_todo]），所以正文始终是唯一数据源，
         不需要真的 GtkCheckButton。 *)
      let base =
        if checked then [ `Color "#26a269" ] else [ `Color "#77767b" ]
      in
      push ~todo:line buf (if checked then "☑ " else "☐ ") base;
      inline_runs buf (if checked then `Strike :: base else base) text;
      (* 换行不给 base 属性（换行没有字形，颜色无所谓）：这样复选框
         不会和后面的空行合并成一段，点击范围才不会溢出到下一行 *)
      push buf "\n" []
  | Markdown.Code code ->
      List.iteri
        (fun i l ->
          if i > 0 then push buf "\n" [];
          push buf l [ `Mono; `Code ])
        (String.split_on_char '\n' code);
      push buf "\n" []

let to_runs (doc : Markdown.doc) : run list =
  let buf = ref [] in
  List.iter (block_runs buf) doc;
  (* 去首尾空白，避免预览顶部出现空行 *)
  let runs = List.rev !buf in
  let rec drop_leading_blanks ls =
    match ls with
    | { text = t; attrs = []; todo_line = _ } :: rest when String.trim t = "" ->
        drop_leading_blanks rest
    | _ -> ls
  in
  drop_leading_blanks runs

let is_empty_runs runs =
  List.for_all (fun r -> String.trim r.text = "") runs

(* ---------- Pango markup（供 tooltip / 测试 / 导出用） ---------- *)

let escape_markup = Markdown.escape_markup

let open_tag_of_attr = function
  | `Bold -> "<b>"
  | `Italic -> "<i>"
  | `Strike -> "<s>"
  | `Underline -> "<span underline=\"single\">"
  | `Baseline -> "<span rise=\"4000\">"
  | `Size n -> Printf.sprintf "<span size=\"%d\">" n
  | `Color c -> Printf.sprintf "<span foreground=\"%s\">" c
  | `Indent n -> Printf.sprintf "<span indent=\"%dpx\">" n
  | `Mono -> "<span font_family=\"Monospace\">"
  | `Code -> "<span background=\"#f6f5f4\">"

let close_tag_of_attr = function
  | `Bold -> "</b>"
  | `Italic -> "</i>"
  | `Strike -> "</s>"
  | `Underline | `Baseline | `Size _ | `Color _ | `Indent _ -> "</span>"
  | `Mono | `Code -> "</span>"

let markup_of_run buf (r : run) =
  List.iter (fun a -> Buffer.add_string buf (open_tag_of_attr a)) r.attrs;
  Buffer.add_string buf (escape_markup r.text);
  List.iter
    (fun a -> Buffer.add_string buf (close_tag_of_attr a))
    (List.rev r.attrs)

let to_markup (doc : Markdown.doc) : string =
  let runs = to_runs doc in
  if is_empty_runs runs then "<span foreground=\"#999999\">（空笔记）</span>"
  else
    let buf = Buffer.create 4096 in
    List.iter (markup_of_run buf) runs;
    String.trim (Buffer.contents buf)

(* ---------- Gtk TextTag 渲染（view 层用） ---------- *)

(** 把 run 序列渲染进 Gtk TextView 的 buffer。

    刻意不经过 markup 字符串：GTK3 的 TextView 不解析 Pango markup，
    只能贴 TextTag，而 AST 到 tag 的映射比"生成 markup 再解析回来"
    少一层不可信的中间表示。 *)
