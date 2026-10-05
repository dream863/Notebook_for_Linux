(** 简易输入对话框：单个带标签的输入框 + 确定/取消。

    只覆盖"文本输入"这一种场景，所以没有用 Gtk 的构造器，直接拼。
    唯一需要特别小心的是口令输入：必须能关回显。 *)

type result = Ok of string | Cancelled

let button_box_row buttons =
  let box = GPack.hbox ~spacing:8 ~homogeneous:true () in
  List.iter (fun b -> ignore (box#pack ~from:`START ~expand:true (b :> GObj.widget))) buttons;
  box#coerce

let run ?parent ?title ?(label = "输入") ?(initial = "") ?password ?(ok_label = "确定") () =
  let dlg =
    GWindow.dialog ~title:(Option.value ~default:"输入" title)
      ?parent:(parent :> GWindow.window_skel option)
      ~modal:true ~resizable:false ~destroy_with_parent:true ~border_width:8 ()
  in
  let content = dlg#vbox in
  ignore (content#set_spacing 8);
  let entry = GEdit.entry ~text:initial ~activates_default:true () in
  (match password with
   | Some true -> (* 关回显：口令不该以明文出现在屏幕上 *) entry#set_visibility false
   | None | Some false -> ());
  let row = GPack.hbox ~spacing:8 () in
  ignore (row#pack ~from:`START (GMisc.label ~xalign:0. ~text:label ())#coerce);
  ignore (row#pack ~from:`START ~expand:true entry#coerce);
  ignore (content#pack ~from:`START ~fill:true row#coerce);
  let cancel = GButton.button ~label:"取消" ~relief:`NONE () in
  let ok = GButton.button ~label:ok_label ~relief:`NONE () in
  ignore (content#pack ~from:`END ~fill:true
            (button_box_row [ cancel; ok ]));
  ignore (cancel#connect #clicked ~callback:(fun () -> dlg#response `CANCEL));
  ignore (ok#connect #clicked ~callback:(fun () -> dlg#response `OK));
  dlg#misc#show_all ();
  let response = dlg#run () in
  let out =
    if response = `CANCEL || response = `DELETE_EVENT then Cancelled
    else Ok entry#text
  in
  dlg#destroy ();
  out

let run_password ?parent ?title () =
  run ?parent ?title ~label:"口令" ~password:true ~ok_label:"解锁" ()

let confirm ?parent ?title ~question () =
  let dlg =
    GWindow.dialog ~title:(Option.value ~default:"确认" title)
      ?parent:(parent :> GWindow.window_skel option)
      ~modal:true ~resizable:false ~destroy_with_parent:true ~border_width:8 ()
  in
  ignore (dlg#vbox#set_spacing 8);
  ignore
    (dlg#vbox#pack ~from:`START ~fill:true
       (GMisc.label ~xalign:0. ~line_wrap:true ~text:question ())#coerce);
  let cancel = GButton.button ~label:"取消" ~relief:`NONE () in
  let ok = GButton.button ~label:"确定" ~relief:`NONE () in
  ignore (dlg#vbox#pack ~from:`END ~fill:true (button_box_row [ cancel; ok ]));
  ignore (cancel#connect #clicked ~callback:(fun () -> dlg#response `CANCEL));
  ignore (ok#connect #clicked ~callback:(fun () -> dlg#response `OK));
  dlg#misc#show_all ();
  let response = dlg#run () in
  dlg#destroy ();
  response = `OK

(** 只放一句话 + 一个关闭按钮。标题栏已经说明了上下文，正文只放正文。 *)
let message_dialog ?parent ?title ~close_label ~message () =
  let dlg =
    GWindow.dialog ~title:(Option.value ~default:"备忘录" title)
      ?parent:(parent :> GWindow.window_skel option)
      ~modal:true ~resizable:false ~destroy_with_parent:true ~border_width:8 ()
  in
  ignore (dlg#vbox#set_spacing 8);
  ignore
    (dlg#vbox#pack ~from:`START ~fill:true
       (GMisc.label ~xalign:0. ~line_wrap:true ~text:message ())#coerce);
  let close = GButton.button ~label:close_label ~relief:`NONE () in
  ignore (dlg#vbox#pack ~from:`END ~fill:true (button_box_row [ close ]));
  ignore (close#connect #clicked ~callback:(fun () -> dlg#response `CLOSE));
  dlg#misc#show_all ();
  ignore (dlg#run ());
  dlg#destroy ()

(** 出错提示。 *)
let error ?parent ?title ~message () =
  message_dialog ?parent ?title ~close_label:"关闭" ~message ()

(** 成功/告知性提示。 *)
let info ?parent ?title ~message () =
  message_dialog ?parent ?title ~close_label:"好" ~message ()

(** 把异常转成一句人话。 *)
let report ?parent ?title e =
  let msg =
    match e with
    | Sqlite_store.Db_error m -> m
    | Crypto.Crypto_exn Crypto.Bad_password -> "口令错误，或数据已损坏"
    | e -> Printexc.to_string e
  in
  error ?parent ?title ~message:msg ()