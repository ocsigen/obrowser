open JSOO
open Js

let _ =
  let body = Node.document >>> get "body" in
  let textbox = Html.create "textarea" ~attrs:["rows","20";"cols","80"] () in
  let preview = Html.div ~style:"border:1px black dashed ; padding: 5px;" [] in
  let rec dyn_preview () =
    let text = textbox >>> get "value" >>> as_string in
    let rendered = Wiki_syntax_client.xml_of_wiki () () text in
      Node.empty preview ;
      List.iter (Node.append preview) rendered ;
      Thread.delay 1. ;
      dyn_preview ()
  in
    Node.append body textbox ;
    Node.append body (Html.br ()) ;
    Node.append body (Node.text "preview computed every second") ;
    Node.append body (Html.br ()) ;
    Node.append body preview ;
    Thread.create dyn_preview ()
;;
