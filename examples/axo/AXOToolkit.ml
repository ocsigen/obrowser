(* This module agregates graphical standard constructions *)


open AXOLang
exception Interrupted

let foldable ?(folded = true) ~button content =
  let container = new AXOWidgets.block_container in
    container#add_widget
      (button :> AXOWidgets.generic AXOWidgets.generic_widget) ;
    if folded then () else container#add_widget content ;
    button#add_click_action
      (fun () ->
         (if button#get_state
          then container#add_widget content
          else container#remove_widget content ) ;
      ) ;
    container

