(* This module agregates graphical standard constructions *)


open JSOO


let foldable ?(folded = true) ~button ?button_alt content =
  let container = AXOHtml.Low.div
                    ~children:(button
                               :: (if folded
                                   then []
                                   else [ content ])
                    )
                    ()
  in
  let folded = ref folded in

    button >>> AXOEvents.Onclick.bind
      (fun () ->
         (if !folded
          then container >>> AXOJs.Node.append content
          else container >>> AXOJs.Node.remove content) ;
         folded := not !folded ;
         (match button_alt with
            | None -> ()
            | Some alt -> button >>> alt)
      ) ;

    container

(* TODO: update to the new AXOTree tree type
let ul_based_tree
      ~button_renderer
      ?fake_button_renderer
      ~node_renderer
      tree =
  let rec aux = function
    | AXOTree.Nil -> AXOHtml.Low.div ()
    | AXOTree.Node (t, []) ->
        AXOHtml.Low.div
          ~children:(
            match fake_button_renderer with
              | None -> [ (node_renderer t []) ; ]
              | Some f -> [ (f t) # get_obj ; (node_renderer t []) ; ]
          )
          ()
    | AXOTree.Node (t, l) ->
        begin
          let button = button_renderer t l in
          let line = node_renderer t l in
          let children = List.map aux l in
          let result =
            AXOHtml.Low.div
              ~children:[ button # get_obj ; line ; ]
              ()
          in
          let wraped_children = AXOHtml.High.ul children in
          let folded = ref true in
            button # get_obj >>> Onclick.bind
              (fun () ->
                 (if !folded
                  then result >>> AXOJs.Node.append wraped_children
                  else result >>> AXOJs.Node.remove wraped_children) ;
                 folded := not !folded ;
                 button # click ) ;
            result
        end
  in aux tree
 *)

type 'a dnd_rendered =
    { dnd_node        : 'a ;            (* a node to keep some info           *)
      dnd_line        : JSOO.obj ;      (* the whole rendrered line           *)
      dnd_dragg       : JSOO.obj ;      (* the handle to drag the line        *)
      dnd_drop        : JSOO.obj ;      (* the place to drop the lines        *)
      dnd_kids_ground : JSOO.obj ;      (* the DOM node to drop children in   *)
      dnd_action      : 'a dnd_rendered AXOTree.tree ->
                        'a dnd_rendered AXOTree.tree ->
                        'a dnd_rendered AXOTree.tree -> unit;

    }
type 'a dnd_rendered_tree = 'a dnd_rendered AXOTree.tree

exception Interrupted of exn

let dndable_tree (*Still buggy*)
      ~renderer   (* take a node, make a rendered *)
      tree =

  let rec aux depth = function
    | { AXOTree.content = t ; AXOTree.children = []} ->
        let result = renderer t [] depth in
          AXOTree.node result []

    | { AXOTree.content = t ; AXOTree.children = l} ->
        let result = renderer t l depth in
        let children = List.map (aux (succ depth)) l in
          List.iter
            (fun c -> result.dnd_kids_ground >>> AXOJs.Node.append c)
            (List.map (fun e -> e.AXOTree.content.dnd_line) children) ;
          AXOTree.node result children

  in

  let tree = ref (aux 0 tree) in

  let rec mu_handler downed downed_c uped uped_c = fun _ ->
    (* unbinding the handler *)
    (try
      AXOTree.iter
           (fun r _ ->
              r.dnd_drop >>> AXOEvents.Mouse_up.clear () ;
           )
        !tree
    with exc ->
    AXOJs.debug (Printexc.to_string exc)) ;

    if uped <> downed
    then (
      let dnode = AXOTree.node downed downed_c in
      let unode = AXOTree.node uped uped_c in
      (* acting as told *)
      (try
         uped.dnd_action !tree dnode unode;
      with exc ->
         AXOJs.alert
           ("drag and drop action failed : " ^ Printexc.to_string exc)) ;
      (try
         tree := AXOTree.move !tree dnode unode ;
         with exc ->
           AXOJs.alert
              "drag and drop action performed, tree manipulation error") ;
      (try
         uped.dnd_kids_ground >>> AXOJs.Node.append downed.dnd_line
       with exc ->
         AXOJs.alert
           "drag and drop action performed, DOM manipulation error. \
            Drag and drop not supported anymore." ;
         AXOTree.iter
           (fun r _ -> r.dnd_dragg >>> AXOEvents.Mouse_down.clear ())
           !tree ;
      )
    )
  and md_handler downed downed_c = fun (mx,my) ->

    AXOTree.iter
      (fun uped uped_c ->
         uped.dnd_drop >>> AXOEvents.Mouse_up.bind
           (mu_handler downed downed_c uped uped_c);
      )
      !tree ;
  in
    AXOTree.iter
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
    | { AXOTree.content = t ; AXOTree.children = []} ->
        let result = renderer t [] depth in
          AXOTree.node result []

    | { AXOTree.content = t ; AXOTree.children = l} ->
        let result = renderer t l depth in
        let children = List.map (aux (succ depth)) l in
          AXOTree.node result children

  in
  let tree = aux 0 tree in
    AXOTree.iter
      (fun { static_line = l } _ -> table >>> AXOJs.Node.append l)
      tree ;
    tree
