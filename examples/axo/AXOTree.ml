
(* tree operations *) (*TODO : optimizations !*)

type 'a tree = { content : 'a ; children : 'a tree list }
exception Empty_tree

let node n l = { content = n ; children = l }
let get_content { content = n } = n
let get_children { children = l } = l

let iter f tree =
  let rec aux { content = t ; children = l } = f t l ; List.iter aux l in
    aux tree
let iteri f tree =
  let rec aux i { content = t ; children = l } = f t l i ; List.iter (aux i) l
  in aux 0 tree

let find f tree =
  let rec aux { content = t ; children = l } =
    if f t l then node t l else auxaux l
  and auxaux = function
    | [] -> raise Not_found
    | hd::tl -> try aux hd with Not_found -> auxaux tl
  in aux tree

let get_parent (tree : 'a tree) (n : 'a tree) : 'a tree =
  find (fun _ l -> List.mem n l) tree

let get_depth (tree : 'a tree) (n : 'a tree) : int =
  let rec aux depth { content = t ; children = l } =
    if t = get_content n then depth else auxaux (succ depth) l
  and auxaux depth = function
    | [] -> raise Not_found
    | hd::tl -> try aux depth hd with Not_found -> auxaux (succ depth) tl
  in aux 0 tree

let map f tree =
  let rec aux { content = t ; children = l } = 
        let (t,l) = f t l in
        let l = List.map aux l in
          node t l 
  in aux tree

let filter f tree =
  let rec aux { content = t ; children = l } =
        if f t l then Some (node t  (AXOLang.filter_map aux l)) else None
  in
    AXOLang.unopt ~exc:Empty_tree (aux tree)

let insert f tree n =
  map (fun t l -> (t, if f t l then n::l else l)) tree
let insert_at tree n d =
  let d = get_content d in
  let rec aux { content = t ; children = l } =
    if t = d then node t (n :: l) else node t (auxaux [] l)
  and auxaux acc = function
    | [] -> List.rev acc
    | hd::tl -> auxaux ((aux hd) :: acc) tl
  in aux tree

let move tree n d =
  insert_at (filter (fun nn l -> nn <> n.content) tree) n d

let to_list tree =
  let rec aux { content = t ; children = l } =
    t :: (List.flatten (List.map aux l))
  in aux tree

