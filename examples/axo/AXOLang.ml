(*These are miscellianous functions to factor code*)

let (>>>) x f = f x
exception Interrupted

module LOption = struct
  let apply_on_opted f = function
    | None -> None
    | Some v -> Some (f v)

  let apply_opted f v = match f with
    | None -> None
    | Some f -> Some (f v)

  let cb_on_opt f = function
    | None -> ()
    | Some v -> f v ; ()

  let unopt ?exc ?default vopt = match (default, vopt) with
    | _, Some v -> v
    | Some v, _ -> v
    | _         -> match exc with
        | None -> failwith "AXOLang.unopt can't unopt None"
        | Some exc -> raise exc

  let assoc_opt k l = try Some (List.assoc k l) with Not_found -> None

  (*TODO: use regexp to have 'none' and 'quote' really mattering*)
  let string_of_t_opt ?(none = "") ?(quote = "") string_of_t = function
    | None -> none
    | Some t -> quote ^ (string_of_t t) ^ quote
  let t_opt_of_string ?(none = "") ?(quote = "") t_of_string = function
    | "" -> None
    | s -> t_of_string s

end

module LList = struct

let filter_map f l =
  let rec aux accu = function
    | [] -> accu
    | h::t -> (match f h with
                 | None -> aux accu t
                 | Some v -> aux (v::accu) t)
  in aux [] l

let split_map f l =
  let rec aux acc1 acc2 = function
    | [] -> (acc1, acc2)
    | hd :: tl -> let (a, b) = f hd in aux (a :: acc1) (b :: acc2) tl
  in aux [] [] l

end

module LTree = struct

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
    let rec aux i { content = t ; children = l } =
      f t l i ;
        List.iter (aux (succ i)) l
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
      if f t l then Some (node t  (LList.filter_map aux l)) else None
    in
      LOption.unopt ~exc:Empty_tree (aux tree)

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

  let is_in_lineage parent child =
    let rec aux children =
      List.mem child children || auxaux children
    and auxaux = function
      | [] -> false
      | { children = c } :: tl -> aux c || auxaux tl
    in aux (get_children parent)

end
