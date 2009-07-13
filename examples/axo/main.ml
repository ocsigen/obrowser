
open AXOLang


let body = AXOJs.Node.document >>> AXOJs.Node.get_element_by_id "body"

let test_tree =
  LTree.node "test"
   [ LTree.node "Un"
      [ LTree.node "toto" [] ;
        LTree.node "fait" [] ;
        LTree.node "la"   [] ;
        LTree.node "cuisine" [] ;
      ] ;
     LTree.node "2"
      [ LTree.node "sdjrhg" [] ;
        LTree.node "fadsrgr" [] ;
        LTree.node "cdsfssdise" [] ;
      ] ;
     LTree.node "3"
      [] ;
     LTree.node "Cinq"
      [ LTree.node "sdjrhg" [] ;
        LTree.node "fgr" [] ;
        LTree.node "cdsfssdise"
         [ LTree.node "haha"
            [ LTree.node "hihi"
                [ LTree.node "hoho"
                    [ LTree.node "huhu" []
                    ] ;
                ] ;
            ] ;
         ];
      ] ;
   ]


let renderer =
    fun t l depth ->

      (* buttons (w and w/o children) *)
      let button = new AXOWidgets.cyclic_button (AXOHtml.Low.span ())
                      (AXOJs.Node.text "> ", true )
                     [(AXOJs.Node.text "v ", false)]
      in
      let button_dummy = new AXOWidgets.text_button ~activated:false "x " in

      (* JSOO.obj's *)
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
                      AXOJs.Node.text t ;
          ]
          ()
      in
      let line = AXOHtml.Low.div
                   ~children:[ content ; ]
                   ()
      in

        button # add_click_action
           (fun _ ->
              if button#get_state
              then line >>> AXOJs.Node.append kids_ground
              else line >>> AXOJs.Node.remove kids_ground
           ) ;

        { AXOToolkit.dnd_node        =
                (t, button, button_dummy, content)  ;
          AXOToolkit.dnd_line        =         line ;
          AXOToolkit.dnd_dragg       =      content ;
          AXOToolkit.dnd_drop        =      content ;
          AXOToolkit.dnd_kids_ground =  kids_ground ;
        }

let action tree dnode unode =
  let (ut, ubut, ubut_dum, ucon) =
    (LTree.get_content unode).AXOToolkit.dnd_node
  in

  AXOJs.debug "toto" ;
  (* getting the dropped node's button right *)
  (match LTree.get_children unode with
     | [] ->
         ucon >>> AXOJs.Node.insert_before ubut#get_obj ubut_dum#get_obj ;
         ucon >>> AXOJs.Node.remove ubut_dum#get_obj
     | _::_ -> ()
  );

  AXOJs.debug "tata" ;
  (* getting the dragged node's parent's button right *)
  (let p = LTree.get_parent tree dnode in
  AXOJs.debug "tutu" ;
    match LTree.get_children p with
     | _::[] ->
         let (_, pbut, pbut_dum, pcon) =
           (LTree.get_content p).AXOToolkit.dnd_node
         in
         pcon >>> AXOJs.Node.insert_before pbut_dum#get_obj pbut#get_obj ;
         pcon >>> AXOJs.Node.remove pbut#get_obj
     | [] | _::_::_ -> ()
  );

  AXOJs.debug "tete" ;
  (* getting the depth right *)
  (let nd = succ (LTree.get_depth tree unode) in
     LTree.iteri
       (fun { AXOToolkit.dnd_node = (_,_,_,c) } _ i ->
          c >>> AXOJs.Node.set_attribute "style"
            ("padding-left: "^(string_of_int (15 * (i + nd)))^"px")
       )
       dnode
  )
 


let dnd_tree = AXOToolkit.dndable_tree ~renderer ~action test_tree


let _ = AXOJs.Misc.disable_selection ()
let _ = body >>> AXOJs.Node.append
          (LTree.get_content (dnd_tree ())).AXOToolkit.dnd_line


let container = AXOHtml.Low.div ()
let parse (_, m) = AXOHtml.Low.div ~children:[AXOJs.Node.text m] ()
let url = "http://localhost:8080/post"
let args = let c = ref 0 in fun () -> incr c ; [("value", string_of_int !c)]
let dl = AXOToolkit.dynamic_list ~container ~parse ~url ~args ()
let _ = body >>> AXOJs.Node.append dl

