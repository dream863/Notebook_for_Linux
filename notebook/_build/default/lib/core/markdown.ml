(** Markdown 解析：只做"切块 + 识别行内样式"，不生成 HTML。

    输出目标不是浏览器而是 Pango markup，所以结构很轻：
    每行解析成一个带样式的片段序列。这让我们完全掌控
    待办 checkbox 的行号映射（方案里的坑 2），而不必引入
    一个 alpha 版的 omd。

    关键约定：待办的勾选状态不单独存 DB，源码是唯一数据源。
    [block.todo] 携带 [line]（0 起的行号）和 [checked]，
    预览层点击 checkbox 时据此反查并改写源码对应行。 *)

type inline =
  | Plain of string
  | Bold of inline list
  | Italic of inline list
  | Code of string
  | Link of string * inline list
  | Strike of inline list

type todo = { line : int; checked : bool; text : inline list }

type block =
  | Heading of int * inline list  (** 级别 1..6 *)
  | Paragraph of inline list
  | Bullet of inline list  (** 无序列表项 *)
  | Ordered of int * inline list  (** 序号 + 有序列表项 *)
  | Todo of todo
  | Code of string  (** 围栏代码块内容 *)
  | Quote of inline list
  | Rule  (** 分隔线 --- *)
  | Blank

type doc = block list

(* ---------- 行内解析 ---------- *)

let is_space c = c = ' ' || c = '\t'

(* Pango markup 的元字符。Markdown 正文里出现 < > & 时若不转义，
   预览窗格会直接解析失败甚至崩掉 —— 这是方案里点名的坑 3。 *)
let escape_markup s =
  let b = Buffer.create (String.length s + 16) in
  String.iter
    (fun c ->
      match c with
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '\'' -> Buffer.add_string b "&apos;"
      | '"' -> Buffer.add_string b "&quot;"
      | _ -> Buffer.add_char b c)
    s;
  Buffer.contents b

let rec parse_inline s =
  let n = String.length s in
  let buf = Buffer.create n in
  let acc = ref [] in
  let flush () =
    if Buffer.length buf > 0 then (
      acc := Plain (Buffer.contents buf) :: !acc;
      Buffer.clear buf)
  in
  let i = ref 0 in
  let starts p = !i + String.length p <= n && String.sub s !i (String.length p) = p in
  while !i < n do
    if starts "**" then begin
      (* 加粗：找到配对的 ** *)
      flush ();
      let rest = String.sub s (!i + 2) (n - !i - 2) in
      match String.index_opt rest '*' with
      | Some k when k + 1 < String.length rest && rest.[k + 1] = '*' ->
          let inner = String.sub rest 0 k in
          acc := Bold (parse_inline inner) :: !acc;
          i := !i + 2 + k + 2
      | _ ->
          Buffer.add_string buf "**";
          i := !i + 2
    end
    else if starts "__" then begin
      flush ();
      let rest = String.sub s (!i + 2) (n - !i - 2) in
      match String.index_opt rest '_' with
      | Some k when k < String.length rest ->
          let inner = String.sub rest 0 k in
          acc := Bold (parse_inline inner) :: !acc;
          i := !i + 2 + k + 1
      | _ ->
          Buffer.add_string buf "__";
          i := !i + 2
    end
    else if starts "~~" then begin
      flush ();
      let rest = String.sub s (!i + 2) (n - !i - 2) in
      match String.index_opt rest '~' with
      | Some k when k + 1 < String.length rest && rest.[k + 1] = '~' ->
          let inner = String.sub rest 0 k in
          acc := Strike (parse_inline inner) :: !acc;
          i := !i + 2 + k + 2
      | _ ->
          Buffer.add_string buf "~~";
          i := !i + 2
    end
    else if starts "*" || starts "_" then begin
      (* 斜体：单星号不跨空白，避免把列表符号吃掉 *)
      let marker = if starts "*" then '*' else '_' in
      let ml = 1 in
      match String.index_from_opt s (!i + ml) marker with
      | Some k
        when k > !i + ml
             && not (is_space s.[!i + ml])
             && String.sub s (!i + ml) (k - !i - ml)
                |> String.trim = String.sub s (!i + ml) (k - !i - ml) ->
          flush ();
          let inner = String.sub s (!i + ml) (k - !i - ml) in
          acc := Italic (parse_inline inner) :: !acc;
          i := k + ml
      | _ ->
          Buffer.add_char buf s.[!i];
          incr i
    end
    else if starts "`" then begin
      flush ();
      match String.index_from_opt s (!i + 1) '`' with
      | Some k ->
          acc := Code (String.sub s (!i + 1) (k - !i - 1)) :: !acc;
          i := k + 1
      | None ->
          Buffer.add_char buf '`';
          incr i
    end
    else if starts "[" then begin
      match String.index_from_opt s (!i + 1) ']' with
      | Some close when close + 1 < n && s.[close + 1] = '(' ->
          (match String.index_from_opt s (close + 2) ')' with
           | Some paren ->
               flush ();
               let url = String.sub s (close + 2) (paren - close - 2) in
               let label = String.sub s (!i + 1) (close - !i - 1) in
               acc := Link (url, parse_inline label) :: !acc;
               i := paren + 1
           | None ->
               Buffer.add_char buf s.[!i];
               incr i)
      | _ ->
          Buffer.add_char buf s.[!i];
          incr i
    end
    else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  flush ();
  List.rev !acc

(* ---------- 块级解析 ---------- *)

let starts_with p s =
  let lp = String.length p in
  String.length s >= lp && String.sub s 0 lp = p

let leading_spaces s =
  let rec go i =
    if i < String.length s && s.[i] = ' ' then go (i + 1) else i
  in
  go 0

let is_blank s = String.trim s = ""

let heading_level s =
  let n = leading_spaces s in
  if n > 3 then None
  else begin
    let body = String.sub s n (String.length s - n) in
    let hashes = ref 0 in
    while !hashes < String.length body && body.[!hashes] = '#' do
      incr hashes
    done;
    if !hashes >= 1 && !hashes <= 6 then
      let after = String.sub body !hashes (String.length body - !hashes) in
      if String.trim after <> "" && (after = "" || is_space after.[0]) then
        Some (!hashes, String.trim after)
      else None
    else None
  end

(** 识别 `- [ ] ` / `- [x] `，返回 (缩进, 是否已完成, 剩余文本)。

    只接受一个空格或一个制表符紧跟方括号，否则 `- [ ]xx` 这类
    普通行内文本会被误判成待办。 *)
let todo_marker s =
  let n = leading_spaces s in
  let body = String.sub s n (String.length s - n) in
  (* 两种标记都是定长 3 字节："[ ]" 与 "[x]"，所以正文一律从第 5 字节开始 *)
  if
    String.length body >= 5
    && (body.[0] = '-' || body.[0] = '*' || body.[0] = '+')
    && is_space body.[1]
    && body.[2] = '['
    && body.[4] = ']'
  then
    let tail = String.sub body 5 (String.length body - 5) in
    if not (tail = "" || is_space tail.[0]) then None
    else
      match body.[3] with
      | ' ' -> Some (n, false, String.trim tail)
      | 'x' | 'X' -> Some (n, true, String.trim tail)
      | _ -> None
  else None

let bullet_marker s =
  let n = leading_spaces s in
  let body = String.sub s n (String.length s - n) in
  if String.length body >= 2 && (body.[0] = '-' || body.[0] = '*' || body.[0] = '+')
     && (body.[1] = ' ' || body.[1] = '\t')
  then
    let tail = String.trim (String.sub body 2 (String.length body - 2)) in
    if tail = "" || tail = "-" || tail = "*" || tail = "+" then None else Some (n, tail)
  else None

let ordered_marker s =
  let n = leading_spaces s in
  let body = String.sub s n (String.length s - n) in
  let len = String.length body in
  let rec digits i = if i < len && body.[i] >= '0' && body.[i] <= '9' then digits (i + 1) else i in
  let d = digits 0 in
  if d > 0 && d + 1 < len && (body.[d] = '.' || body.[d] = ')')
     && (body.[d + 1] = ' ' || body.[d + 1] = '\t')
  then
    let num = int_of_string (String.sub body 0 d) in
    let tail = String.trim (String.sub body (d + 2) (len - d - 2)) in
    Some (n, num, tail)
  else None

let quote_marker s =
  let n = leading_spaces s in
  let body = String.sub s n (String.length s - n) in
  if String.length body >= 2 && body.[0] = '>' && (body.[1] = ' ' || body.[1] = '\t')
  then Some (n, String.trim (String.sub body 1 (String.length body - 1)))
  else if String.length body >= 1 && body.[0] = '>' then Some (n, "")
  else None

let is_rule s =
  let t = String.trim s in
  String.length t >= 3
  &&
  let rec all c i = if i >= String.length t then true else t.[i] = c && all c (i + 1) in
  (all '-' 0 || all '*' 0 || all '_' 0)

let parse src =
  let lines = String.split_on_char '\n' src in
  let blocks = ref [] in
  let in_code = ref false in
  let code_buf = Buffer.create 256 in
  let code_lang = ref "" in
  let flush_code () =
    if !in_code then (
      blocks := Code (Buffer.contents code_buf) :: !blocks;
      Buffer.clear code_buf;
      in_code := false;
      ignore !code_lang)
  in
  List.iteri
    (fun idx line ->
      let t = String.trim line in
      if starts_with "```" t then begin
        if !in_code then flush_code ()
        else begin
          in_code := true;
          code_lang := String.trim (String.sub t 3 (String.length t - 3))
        end
      end
      else if !in_code then begin
        Buffer.add_string code_buf line;
        Buffer.add_char code_buf '\n'
      end
      else if is_blank line then blocks := Blank :: !blocks
      else if is_rule line then blocks := Rule :: !blocks
      else
        match heading_level line with
        | Some (lvl, txt) -> blocks := Heading (lvl, parse_inline txt) :: !blocks
        | None -> (
            match todo_marker line with
            | Some (_ind, checked, txt) ->
                blocks :=
                  Todo { line = idx; checked; text = parse_inline txt } :: !blocks
            | None -> (
                match bullet_marker line with
                | Some (_ind, txt) -> blocks := Bullet (parse_inline txt) :: !blocks
                | None -> (
                    match ordered_marker line with
                    | Some (_ind, num, txt) ->
                        blocks := Ordered (num, parse_inline txt) :: !blocks
                    | None -> (
                        match quote_marker line with
                        | Some (_ind, txt) -> blocks := Quote (parse_inline txt) :: !blocks
                        | None -> blocks := Paragraph (parse_inline line) :: !blocks)))))
    lines;
  flush_code ();
  (* code_buf 最后多了一个换行，去掉 *)
  List.rev_map
    (function
      | Code c ->
          let c = if String.length c > 0 && c.[String.length c - 1] = '\n' then
              String.sub c 0 (String.length c - 1)
            else c in
          Code c
      | b -> b)
    !blocks

(* ---------- 待办勾选状态的反写 ----------

   预览态点击 checkbox 时调用：按行号找到源码对应行，
   只翻转 `[ ]` / `[x]`，其余字节原样保留。
   这样源码始终是唯一数据源，不存在两份状态漂移的可能。 *)

let toggle_todo src line =
  let lines = Array.of_list (String.split_on_char '\n' src) in
  if line < 0 || line >= Array.length lines then src
  else
    (* 只允许翻转真正被解析成 Todo 的行：代码块里的 "- [ ]" 或
       普通段落里的 "[ ]" 不会被误改。 *)
    let is_todo =
      List.exists
        (fun b ->
          match b with
          | Todo t -> t.line = line
          | _ -> false)
        (parse src)
    in
    if not is_todo then src
    else
      let line_text = lines.(line) in
      let n = leading_spaces line_text in
      let body = String.sub line_text n (String.length line_text - n) in
      let len = String.length body in
      if
        len >= 6
        && (body.[0] = '-' || body.[0] = '*' || body.[0] = '+')
        && (body.[1] = ' ' || body.[1] = '\t')
        && body.[2] = '['
        && body.[4] = ']'
      then (
        (* 标记永远是 3 字节 "[ ]" / "[x]"，所以固定跳过 5 字节：
           "- " + "[ ]" *)
        let new_marker = if body.[3] = ' ' then 'x' else ' ' in
        let tail = String.sub body 5 (len - 5) in
        (* body 的布局是 "- " + "[ ]"：前缀只取前 2 字节（"- "），
           再拼新的 3 字节标记 *)
        let new_line =
          String.sub line_text 0 n
          ^ String.sub body 0 2
          ^ "[" ^ String.make 1 new_marker ^ "]"
          ^ tail
        in
        lines.(line) <- new_line;
        String.concat "\n" (Array.to_list lines))
      else src

(* ---------- 统计 ---------- *)

type stats = {
  total : int;
  done_ : int;
  pending : int;
}

let stats doc =
  let total = ref 0 and d = ref 0 in
  List.iter
    (function Todo { checked; _ } ->
      incr total;
      if checked then incr d
      | _ -> ())
    doc;
  { total = !total; done_ = !d; pending = !total - !d }
