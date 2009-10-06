(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: camlinternalOO.ml,v 1.16 2008/01/11 16:13:16 doligez Exp $ *)

open Obj

(**** Object representation ****)

let last_id = ref 0
let new_id () =
  let id = !last_id in incr last_id; id

let set_id o id =
  let id0 = !id in
  Array.unsafe_set (Obj.magic o : int array) 1 id0;
  id := id0 + 1

(**** Object copy ****)

let copy o =
  let o = (Obj.obj (Obj.dup (Obj.repr o))) in
  set_id o last_id;
  o

(**** Compression options ****)
(* Parameters *)
type params = {
    mutable compact_table : bool;
    mutable copy_parent : bool;
    mutable clean_when_copying : bool;
    mutable retry_count : int;
    mutable bucket_small_size : int
  }

let params = {
  compact_table = true;
  copy_parent = true;
  clean_when_copying = true;
  retry_count = 3;
  bucket_small_size = 16
}

(**** Parameters ****)

let step = Sys.word_size / 16
let initial_object_size = 2

(**** Items ****)

type item = DummyA | DummyB | DummyC of int

let dummy_item = (magic () : item)

(**** Types ****)

type tag
type label = int
type closure = item
type t = DummyA | DummyB | DummyC of int
type obj = t array
external ret : (obj -> 'a) -> closure = "%identity"

(**** Labels ****)

let public_method_label s : tag =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  let tag = if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu in
  (* Printf.eprintf "%s = %d\n" s tag; flush stderr; *)
  magic tag

(**** Sparse array ****)

(* The compiler assumes that the first field of this structure is [size]. *)
type table

let dummy_table = magic [| |]

external new_table : label array -> table = "oo_new_table"
external put : table -> int -> 'a = "oo_put"

(**** Classes ****)

(* type t *)
type meth = item

external new_method : table -> int = "oo_new_method"
external get_method_label : table -> string -> label = "oo_get_method_label"

let get_method_labels table names =
  Array.map (get_method_label table) names

external set_method : table -> label -> 'a -> unit = "oo_set_method"
external get_method : table -> label -> 'a = "oo_get_method"

external narrow : table -> 'a array -> 'b array -> 'c array -> unit = "oo_narrow"
external widen : table -> unit = "oo_widen"
  
external new_slot : table -> int = "oo_new_slot"
external new_variable : table -> string -> int = "oo_new_variable"

let to_array a = if a = magic 0 then [||] else magic a

let new_methods_variables table meths vals =
  let meths = to_array meths in
  let nmeths = Array.length meths and nvals = Array.length vals in
  let res = Array.create (nmeths + nvals) 0 in
  for i = 0 to nmeths - 1 do
    res.(i) <- get_method_label table meths.(i)
  done;
  for i = 0 to nvals - 1 do
    res.(i+nmeths) <- new_variable table vals.(i)
  done;
  res

external get_variable : table -> string -> int = "oo_get_variable"

let get_variables table names =
  Array.map (get_variable table) names

external initializers : table -> (obj -> unit) list ref = "oo_initializers"

let add_initializer table f =
  initializers table := f :: !(initializers table)

external create_table : string array -> table = "oo_create_table"
external init_class_raw : table -> unit = "oo_init_class_raw"

let init_class table =
  initializers table := List.rev !(initializers table);
  init_class_raw table

let inherits cla vals virt_meths concr_meths (_, super, _, env) top =
  narrow cla vals virt_meths concr_meths;
  let init =
    if top then super cla env else Obj.repr (super cla) in
  widen cla;
  Array.concat
    [[| repr init |];
     magic (Array.map (get_variable cla) (to_array vals) : int array);
     Array.map
       (fun nm -> repr (get_method cla (get_method_label cla nm) : closure))
       (to_array concr_meths) ]

let make_class pub_meths class_init =
  let table = create_table pub_meths in
  let env_init = class_init table in
  init_class table;
  (env_init (Obj.repr 0), class_init, env_init, Obj.repr 0)

type init_table = { mutable env_init: t; mutable class_init: table -> t }

let make_class_store pub_meths class_init init_table =
  let table = create_table pub_meths in
  let env_init = class_init table in
  init_class table;
  init_table.class_init <- class_init;
  init_table.env_init <- env_init

let dummy_class loc =
  let undef = fun _ -> raise (Undefined_recursive_module loc) in
  (Obj.magic undef, undef, undef, Obj.repr 0)

(**** Objects ****)

external raw_size : table -> int = "oo_size"
external raw_methods : table -> 'a array = "oo_methods"

let create_object table =
  (* XXX Appel de [obj_block] *)
  let obj = Obj.new_block Obj.object_tag (raw_size table) in
  (* XXX Appel de [caml_modify] *)
  Obj.set_field obj 0 (Obj.repr (raw_methods table));
  set_id obj last_id;
  (Obj.obj obj)

let create_object_opt obj_0 table =
  if (Obj.magic obj_0 : bool) then obj_0 else begin
    (* XXX Appel de [obj_block] *)
    let obj = Obj.new_block Obj.object_tag (raw_size table) in
    (* XXX Appel de [caml_modify] *)
    Obj.set_field obj 0 (Obj.repr (raw_methods table));
    set_id obj last_id;
    (Obj.obj obj)
  end

let rec iter_f obj =
  function
    []   -> ()
  | f::l -> f obj; iter_f obj l

let run_initializers obj table =
  let inits = !(initializers table) in
  if inits <> [] then
    iter_f obj inits


let run_initializers_opt obj_0 obj table =
  if (Obj.magic obj_0 : bool) then obj else begin
    let inits = !(initializers table) in
    if inits <> [] then iter_f obj inits;
    obj
  end

let create_object_and_run_initializers obj_0 table =
  if (Obj.magic obj_0 : bool) then obj_0 else begin
    let obj = create_object table in
    run_initializers obj table;
    obj
  end

(* Equivalent primitive below
let sendself obj lab =
  (magic obj : (obj -> t) array array).(0).(lab) obj
*)
external send : obj -> tag -> 'a = "%send"
external sendcache : obj -> tag -> t -> int -> 'a = "%sendcache"
external sendself : obj -> label -> 'a = "%sendself"
external get_public_method : obj -> tag -> closure
    = "caml_get_public_method" "noalloc"

(**** table collection access ****)

type tables = Empty | Cons of closure * tables * tables
type mut_tables =
    {key: closure; mutable data: tables; mutable next: tables}
external mut : tables -> mut_tables = "%identity"

let build_path n keys tables =
  let res = Cons (Obj.magic 0, Empty, Empty) in
  let r = ref res in
  for i = 0 to n do
    r := Cons (keys.(i), !r, Empty)
  done;
  tables.data <- !r;
  res

let rec lookup_keys i keys tables =
  if i < 0 then tables else
  let key = keys.(i) in
  let rec lookup_key tables =
    if tables.key == key then lookup_keys (i-1) keys tables.data else
    if tables.next <> Empty then lookup_key (mut tables.next) else
    let next = Cons (key, Empty, Empty) in
    tables.next <- next;
    build_path (i-1) keys (mut next)
  in
  lookup_key (mut tables)

let lookup_tables root keys =
  let root = mut root in
  if root.data <> Empty then
    lookup_keys (Array.length keys - 1) keys root.data
  else
    build_path (Array.length keys - 1) keys root

(**** builtin methods ****)

let get_const x = ret (fun obj -> x)
let get_var n   = ret (fun obj -> Array.unsafe_get obj n)
let get_env e n =
  ret (fun obj ->
    Array.unsafe_get (Obj.magic (Array.unsafe_get obj e) : obj) n)
let get_meth n  = ret (fun obj -> sendself obj n)
let set_var n   = ret (fun obj x -> Array.unsafe_set obj n x)
let app_const f x = ret (fun obj -> f x)
let app_var f n   = ret (fun obj -> f (Array.unsafe_get obj n))
let app_env f e n =
  ret (fun obj ->
    f (Array.unsafe_get (Obj.magic (Array.unsafe_get obj e) : obj) n))
let app_meth f n  = ret (fun obj -> f (sendself obj n))
let app_const_const f x y = ret (fun obj -> f x y)
let app_const_var f x n   = ret (fun obj -> f x (Array.unsafe_get obj n))
let app_const_meth f x n = ret (fun obj -> f x (sendself obj n))
let app_var_const f n x = ret (fun obj -> f (Array.unsafe_get obj n) x)
let app_meth_const f n x = ret (fun obj -> f (sendself obj n) x)
let app_const_env f x e n =
  ret (fun obj ->
    f x (Array.unsafe_get (Obj.magic (Array.unsafe_get obj e) : obj) n))
let app_env_const f e n x =
  ret (fun obj ->
    f (Array.unsafe_get (Obj.magic (Array.unsafe_get obj e) : obj) n) x)
let meth_app_const n x = ret (fun obj -> (sendself obj n : _ -> _) x)
let meth_app_var n m =
  ret (fun obj -> (sendself obj n : _ -> _) (Array.unsafe_get obj m))
let meth_app_env n e m =
  ret (fun obj -> (sendself obj n : _ -> _)
      (Array.unsafe_get (Obj.magic (Array.unsafe_get obj e) : obj) m))
let meth_app_meth n m =
  ret (fun obj -> (sendself obj n : _ -> _) (sendself obj m))
let send_const m x c =
  ret (fun obj -> sendcache x m (Array.unsafe_get obj 0) c)
let send_var m n c =
  ret (fun obj ->
    sendcache (Obj.magic (Array.unsafe_get obj n) : obj) m
      (Array.unsafe_get obj 0) c)
let send_env m e n c =
  ret (fun obj ->
    sendcache
      (Obj.magic (Array.unsafe_get
                    (Obj.magic (Array.unsafe_get obj e) : obj) n) : obj)
      m (Array.unsafe_get obj 0) c)
let send_meth m n c =
  ret (fun obj ->
    sendcache (sendself obj n) m (Array.unsafe_get obj 0) c)
let new_cache table =
  let n = new_method table in
  let n =
    if n mod 2 = 0 || n > 2 + magic (raw_methods table).(1) * 16 / Sys.word_size
    then n else new_method table
  in
  (raw_methods table).(n) <- Obj.magic 0;
  n

type impl =
    GetConst
  | GetVar
  | GetEnv
  | GetMeth
  | SetVar
  | AppConst
  | AppVar
  | AppEnv
  | AppMeth
  | AppConstConst
  | AppConstVar
  | AppConstEnv
  | AppConstMeth
  | AppVarConst
  | AppEnvConst
  | AppMethConst
  | MethAppConst
  | MethAppVar
  | MethAppEnv
  | MethAppMeth
  | SendConst
  | SendVar
  | SendEnv
  | SendMeth
  | Closure of closure

let method_impl table i arr =
  let next () = incr i; magic arr.(!i) in
  match next() with
    GetConst -> let x : t = next() in get_const x
  | GetVar   -> let n = next() in get_var n
  | GetEnv   -> let e = next() and n = next() in get_env e n
  | GetMeth  -> let n = next() in get_meth n
  | SetVar   -> let n = next() in set_var n
  | AppConst -> let f = next() and x = next() in app_const f x
  | AppVar   -> let f = next() and n = next () in app_var f n
  | AppEnv   ->
      let f = next() and e = next() and n = next() in app_env f e n
  | AppMeth  -> let f = next() and n = next () in app_meth f n
  | AppConstConst ->
      let f = next() and x = next() and y = next() in app_const_const f x y
  | AppConstVar ->
      let f = next() and x = next() and n = next() in app_const_var f x n
  | AppConstEnv ->
      let f = next() and x = next() and e = next () and n = next() in
      app_const_env f x e n
  | AppConstMeth ->
      let f = next() and x = next() and n = next() in app_const_meth f x n
  | AppVarConst ->
      let f = next() and n = next() and x = next() in app_var_const f n x
  | AppEnvConst ->
      let f = next() and e = next () and n = next() and x = next() in
      app_env_const f e n x
  | AppMethConst ->
      let f = next() and n = next() and x = next() in app_meth_const f n x
  | MethAppConst ->
      let n = next() and x = next() in meth_app_const n x
  | MethAppVar ->
      let n = next() and m = next() in meth_app_var n m
  | MethAppEnv ->
      let n = next() and e = next() and m = next() in meth_app_env n e m
  | MethAppMeth ->
      let n = next() and m = next() in meth_app_meth n m
  | SendConst ->
      let m = next() and x = next() in send_const m x (new_cache table)
  | SendVar ->
      let m = next() and n = next () in send_var m n (new_cache table)
  | SendEnv ->
      let m = next() and e = next() and n = next() in
      send_env m e n (new_cache table)
  | SendMeth ->
      let m = next() and n = next () in send_meth m n (new_cache table)
  | Closure _ as clo -> magic clo

let set_methods table methods =
  let len = Array.length methods and i = ref 0 in
  while !i < len do
    let label = methods.(!i) and clo = method_impl table i methods in
    set_method table label clo;
    incr i
  done

(**** Statistics ****)

type stats =
  { classes: int; methods: int; inst_vars: int; }

let stats () =
  assert false
;;
(*  { classes = !table_count;
    methods = !method_count; inst_vars = !inst_var_count; } *)
