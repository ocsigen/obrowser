(*These are miscellianous functions to factor code*)

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
