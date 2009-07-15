(* This module agregates graphical standard constructions *)


open AXOLang
exception Interrupted

let foldable ?(folded = true) ~button content =
  let container = new AXOWidgets.div_container in
    container#add_widget (button :> AXOWidgets.widget) ;
    if folded then () else container#add_widget content ;
    button#add_click_action
      (fun () ->
         (if button#get_state
          then container # add_widget content
          else container # remove_widget content) ;
      ) ;
    container


type 'a dnd_rendered =
    { dnd_node        : 'a ;            (* a node to keep some info           *)
      dnd_line        : JSOO.obj ;      (* the whole rendrered line           *)
      dnd_dragg       : JSOO.obj ;      (* the handle to drag the line        *)
      dnd_drop        : JSOO.obj ;      (* the place to drop the lines        *)
      dnd_kids_ground : JSOO.obj ;      (* the DOM node to drop children in   *)
    }

let dndable_tree (*Still buggy*)
      ~renderer   (* take a node, make a rendered *)
      ~action
      tree =

  let shadow =
    let o = AXOHtml.Low.div
              ~attrs:[("style","background: gray; opacity: .3;")]
              ()
    in
    let s = new AXOWidgets.shadow AXOJs.body o in
      s # set_height 10 ; s # set_width 30 ; s
  in

  let rec aux depth = function
    | { LTree.content = t ; LTree.children = []} ->
        let result = renderer t [] depth in
          LTree.node result []

    | { LTree.content = t ; LTree.children = l} ->
        let result = renderer t l depth in
        let children = List.map (aux (succ depth)) l in
          List.iter
            (fun c -> result.dnd_kids_ground >>> AXOJs.Node.append c)
            (List.map (fun e -> e.LTree.content.dnd_line) children) ;
          LTree.node result children

  in

  let tree = ref (aux 0 tree) in

  let rec mu_handler downed downed_c uped uped_c = fun _ ->
    shadow#deactivate ;

    (* unbinding the handler *)
    (try
      LTree.iter
           (fun r _ ->
              r.dnd_drop >>> AXOEvents.Mouse_up.clear () ;
           )
        !tree
    with exc ->
    AXOJs.debug (Printexc.to_string exc)) ;

    if uped <> downed
    then (
      let dnode = LTree.node downed downed_c in
      let unode = LTree.node uped uped_c in
      (* acting as told *)
      (try
         (try
            action !tree dnode unode;
          with exc ->
            AXOJs.alert
              ("drag and drop action failed : " ^ Printexc.to_string exc) ;
            raise Interrupted
         ) ;
         (try
            tree := LTree.move !tree dnode unode ;
          with exc ->
            AXOJs.alert
              "drag and drop action performed, tree manipulation error.\
               You may want to reload the page." ;
            raise Interrupted
         ) ;
         (try
            uped.dnd_kids_ground >>> AXOJs.Node.append downed.dnd_line
          with exc ->
            AXOJs.alert
              "drag and drop action performed, DOM manipulation error. \
               Drag and drop not supported anymore." ;
            LTree.iter
              (fun r _ -> r.dnd_dragg >>> AXOEvents.Mouse_down.clear ())
              !tree ;
         )
       with Interrupted -> ()
      )
    )
  and md_handler downed downed_c = fun (mx,my) ->
    shadow#activate ;

    LTree.iter
      (fun uped uped_c ->
         uped.dnd_drop >>> AXOEvents.Mouse_up.bind
           (mu_handler downed downed_c uped uped_c);
      )
      !tree ;
  in
    LTree.iter
      (fun r rc -> r.dnd_dragg >>> AXOEvents.Mouse_down.bind (md_handler r rc))
      !tree ;
    (fun () -> !tree)



type 'a static_rendered =
    { static_node        : 'a ;        (* the original node of the tree      *)
      static_line        : JSOO.obj ;  (* the whole rendrered line           *)
    }

let static_table_tree
      ~renderer   (* take a node, make a rendered *)
      ~table
      tree =

  let rec aux depth = function
    | { LTree.content = t ; LTree.children = []} ->
        let result = renderer t [] depth in
          LTree.node result []

    | { LTree.content = t ; LTree.children = l} ->
        let result = renderer t l depth in
        let children = List.map (aux (succ depth)) l in
          LTree.node result children

  in
  let tree = aux 0 tree in
    LTree.iter
      (fun { static_line = l } _ -> table >>> AXOJs.Node.append l)
      tree ;
    tree



let dynamic_list ~container ?preload ?button ?throbber ~parse ~url ~args () =
  let button = LOption.unopt
    ~default:(new AXOWidgets.text_button "Load more...")
    button
  in
  let throbber = LOption.unopt
    ~default:(
      fun () ->
        AXOHtml.Low.div ~children:[AXOJs.Node.text "Waiting..."] ()
    )
    throbber
  in
  let rec load_n_show () =
    try
      let t = throbber () in
      container >>> AXOJs.Node.insert_before t button#get_obj ;
      let res = AXOCom.dynload url (args ()) parse in (*TODO use the real dynload*)
      container >>> AXOJs.Node.insert_before res t ;
      container >>> AXOJs.Node.remove t ;

    with Failure t -> button#remove_click_action load_n_show ; AXOJs.alert t
  in
    button#add_click_action load_n_show ;
    container >>> AXOJs.Node.append button#get_obj ;
    container

