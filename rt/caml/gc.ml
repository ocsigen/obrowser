(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: gc.ml 9131 2008-11-18 10:24:43Z doligez $ *)

type stat = {
  minor_words : float;
  promoted_words : float;
  major_words : float;
  minor_collections : int;
  major_collections : int;
  heap_words : int;
  heap_chunks : int;
  live_words : int;
  live_blocks : int;
  free_words : int;
  free_blocks : int;
  largest_free : int;
  fragments : int;
  compactions : int;
  top_heap_words : int;
};;

type control = {
  mutable minor_heap_size : int;
  mutable major_heap_increment : int;
  mutable space_overhead : int;
  mutable verbose : int;
  mutable max_overhead : int;
  mutable stack_limit : int;
  mutable allocation_policy : int;
};;

let stat _ = failwith "not implemented in obrowser"
let quick_stat _ = failwith "not implemented in obrowser"
let counters _ = failwith "not implemented in obrowser"
let get _  = failwith "not implemented in obrowser"
let set _  = failwith "not implemented in obrowser"
let minor _ = failwith "not implemented in obrowser"
let major_slice _ = failwith "not implemented in obrowser"
let major _ = failwith "not implemented in obrowser"
let full_major _ = failwith "not implemented in obrowser"
let compact _ = failwith "not implemented in obrowser"

let print_stat _ = failwith "not implemented in obrowser"

let allocated_bytes () =
  let (mi, pro, ma) = counters () in
  (mi +. ma -. pro) *. float_of_int (Sys.word_size / 8)
;;

let finalise _ _ = failwith "not implemented in obrowser"
let finalise_release _ = failwith "not implemented in obrowser"


type alarm
type alarm_rec

let rec call_alarm _ = failwith "not implemented in obrowser"

let create_alarm _ = failwith "not implemented in obrowser"

let delete_alarm _ = failwith "not implemented in obrowser"
