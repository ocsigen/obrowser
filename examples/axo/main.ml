
open JSOO

module Onclick =
  AXOEvents.Make
    (struct
       type v = unit
       let name = "onclick"
       let destruct = fun _ -> ()
       let default_value = Some ()
     end)

let body = AXOJs.Node.document >>> AXOJs.Node.get_element_by_id "body"

let test_tree =
  AXOTree.node "test"
   [ AXOTree.node "1"
      [ AXOTree.node "toto" [] ;
        AXOTree.node "fait" [] ;
        AXOTree.node "la"   [] ;
        AXOTree.node "cuisine" [] ;
      ] ;
     AXOTree.node "2"
      [ AXOTree.node "sdjrhg" [] ;
        AXOTree.node "fadsrgr" [] ;
        AXOTree.node "cdsfssdise" [] ;
      ] ;
     AXOTree.node "3"
      [] ;
     AXOTree.node "5"
      [ AXOTree.node "sdjrhg" [] ;
        AXOTree.node "fgr" [] ;
        AXOTree.node "cdsfssdise"
         [ AXOTree.node "haha"
            [ AXOTree.node "hihi" []
            ] ;
         ];
      ] ;
   ]


let renderer =
  let even = ref false in
    fun t l depth ->
      even := not !even ;

      (* buttons (w and w/o children) *)
      let button =
        new AXOWidgets.cyclic_button
          (AXOHtml.Low.span ())
          (AXOJs.Node.text "> ", true)
          [(AXOJs.Node.text "v ", false)]
      in
      let button_dummy = new AXOWidgets.text_button "x " in

      (* JSOO.obj's *)
      let dragg = AXOHtml.Low.span
                    ~attrs:[("style", "background: gray")]
                    ~children:[AXOJs.Node.text "# "]
                    ()
      in
      let kids_ground = AXOHtml.Low.ul
                          ~attrs:[("style", "margin: 0px ; padding: 0px")]
                          ()
      in
      let content =
        AXOHtml.Low.span
          ~attrs:[("style",
                   "padding-left: "^(string_of_int (15 * depth))^"px")]
          ~children:[ (match l with
                         | [] -> button_dummy # get_obj
                         | _::_ -> button # get_obj    ) ;
                      AXOJs.Node.text t]
          ()
      in
      let drop =
            AXOHtml.Low.span
              ~children:[ dragg ; content ; ]
              ()
      in
      let line = AXOHtml.Low.div
              ~attrs:[("style",
                       "background: "
                       ^ if !even then "rgb(250,250,250)" else "rgb(230,230,230)")]
              ~children:[ drop ]
              ()
      in

      let dnd_action t d u =
        (match AXOTree.get_children u with
           | [] -> content >>> AXOJs.Node.insert_before
                      (button # get_obj) (button_dummy # get_obj) ;
                   content >>> AXOJs.Node.remove
                      (button_dummy # get_obj) ;
           | _::_ -> ()
        ) ;
        (match AXOTree.get_children (AXOTree.get_parent t d) with
           | [d] -> content >>> AXOJs.Node.insert_before
                      (button_dummy # get_obj) (button # get_obj) ;
                    content >>> AXOJs.Node.remove
                      (button # get_obj) ;
           | _::_::_ | [] -> ()
        ) ;
        let new_depth = succ (AXOTree.get_depth t u) in
          AXOTree.iteri
            (fun t _ i ->
                snd (t.AXOToolkit.dnd_node) >>> AXOJs.Node.set_attribute "style"
                  ("padding-left: "^(string_of_int (15 * (i + new_depth)))^"px")
            )
            d ;
      in

        (button # get_obj) >>> AXOEvents.Onclick.bind
           (fun _ ->
              button # click ;
              if button#get_state
              then line >>> AXOJs.Node.append kids_ground
              else line >>> AXOJs.Node.remove kids_ground
           ) ;
         (button_dummy # get_obj) >>> AXOEvents.Onclick.bind
            (fun _ -> button_dummy#click) ;

        { AXOToolkit.dnd_node        = (t, content) ;
          AXOToolkit.dnd_line        =         line ;
          AXOToolkit.dnd_dragg       =        dragg ;
          AXOToolkit.dnd_drop        =         drop ;
          AXOToolkit.dnd_kids_ground =  kids_ground ;
          AXOToolkit.dnd_action      =   dnd_action ;
        }

let dnd_tree : unit -> (string * JSOO.obj) AXOToolkit.dnd_rendered_tree =
  AXOToolkit.dndable_tree ~renderer test_tree


let _ = AXOJs.Misc.disable_selection ()
let _ = body >>> AXOJs.Node.append
          (dnd_tree ()).AXOTree.content.AXOToolkit.dnd_line


