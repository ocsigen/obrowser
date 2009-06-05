open JSOO

let window = eval "window"
let document = eval "document"

let alert msg =
  window >>> call_method "alert" [| string msg |] >>> ignore 

let get_element_by_id id =
  document >>> call_method "getElementById" [| string id |]

let create name =
  document >>> call_method "createElement" [| string name |]

let text content =
  document >>> call_method "createTextNode" [| string content |]

