let s = " ==paf" in
let newcontent = Wiki_syntax_client.xml_of_wiki () () s in
  let noeud = Js.get_element_by_id "toto" in
    List.iter (Js.Node.append noeud) newcontent
;;
