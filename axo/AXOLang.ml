(** These are miscellianous functions to factor code.
  * There is nothing specific to obrwoser nor AXO in this module only
  * simple/basic functions that could be included in a course or an
  * extensive base library (such as Batteries).
  *
  * As the modules only agregates classic examples/exercises in ocaml there's
  * not much of a documentation here. *)

let (>>>) x f = f x

let string_of_char c = String.make 1 c

module LOption = struct
  let apply_on_opted f = function
    | None -> None
    | Some v -> Some (f v)

  let apply_opted f v = match f with
    | None -> None
    | Some f -> Some (f v)

  let cb_on_opted f = function
    | None -> ()
    | Some v -> f v ; ()

  let cb_opted f v = match f with
    | None -> ()
    | Some f -> f v ; ()

  let unopt ?(exc = Failure "AXOLang.unopt can't unopt None") ?default vopt =
    match (default, vopt) with
      | _     , Some v -> v
      | Some v, None   -> v
      | None  , None   -> raise exc

  let assoc_opt k l = try Some (List.assoc k l) with Not_found -> None

  let string_of_t_opt string_of_t = function
    | None -> ""
    | Some t -> string_of_t t
  let t_opt_of_string t_of_string = function
    | "" -> None
    | s -> Some (t_of_string s)

  let optionnaly_add_to_list l = function
    | None -> l
    | Some v -> v::l

end

module LList = struct

  (** [rev_append l1 l2 = (List.rev l1) @ l2] ebvaluates to true *)
  let rev_append l1 l2 =
    let rec aux l2 = function
      | [] -> l2
      | hd :: tl -> aux (hd :: l2) tl
    in aux l1 l2

  let filter_map f l = (* ('a -> 'b option) -> 'a list -> 'b list *)
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

  let insert_after l element reference =
    let rec aux acc = function
      | [] -> raise Not_found
      | hd :: tl ->
          if hd = reference
          then rev_append acc ( hd :: element :: tl )
          else aux ( hd :: acc ) tl
    in aux [] l

  let insert_after_ l element func =
    let rec aux acc = function
      | [] -> raise Not_found
      | hd :: tl ->
          if func hd
          then rev_append acc (hd :: element :: tl)
          else aux ( hd :: acc ) tl
    in aux [] l

  let find_remove f l =
    let rec aux acc = function
      | [] -> raise Not_found
      | hd :: tl -> if f hd then (hd,rev_append acc tl) else aux (hd :: acc) tl
    in aux [] l

  let interval_list ?(comp = compare) ~bump ~min ~max () =
    let rec aux accu curr =
      if (comp curr max) > 0
      then accu
      else aux (curr::accu) (bump curr)
    in List.rev (aux [] min) (*TODO : optimize (easy) *)

  let int_interval_list ?(bump = 1) ~min ~max () =
    interval_list ~bump:((+) bump) ~min ~max ()

  let int32_interval_list ?(bump = Int32.one) ~min ~max () =
    interval_list ~bump:(Int32.add bump) ~min ~max ()

  let t_opt_list_of_t_list l = None :: ( List.map (fun v -> Some v) l )

  let map_list_of_array f a =
    let rec aux accu i =
     if i < 0
     then accu
     else aux (( f a.(i) ) :: accu) (pred i)
    in aux [] (pred ( Array.length a ))

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
  (** the function argument receives depth information *)
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

  let sort ?(comp = compare) t =
    let rec aux { content = n ; children = c } =
      node n (List.map aux (List.sort comp c))
    in aux t

end
