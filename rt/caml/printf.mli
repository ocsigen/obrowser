(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*  Xavier Leroy and Pierre Weis, projet Cristal, INRIA Rocquencourt   *)
(*            Modified version for O'Browser by Benjamin Canou         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: printf.mli,v 1.54.6.2 2008/01/11 10:50:06 doligez Exp $ *)

(** Formatted output functions. *)

val sprintf : ('a, unit, string) format -> 'a
(** Same as {!Printf.fprintf}, but instead of printing on an output channel,
   return a string containing the result of formatting the arguments. *)

val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
(** Same as {!Printf.fprintf}, but instead of printing on an output channel,
   append the formatted arguments to the given extensible buffer
   (see module {!Buffer}). *)

(** Formatted output functions with continuations. *)

val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b;;
(** Same as [sprintf] above, but instead of returning the string,
   passes it to the first argument. *)

val kbprintf : (Buffer.t -> 'a) -> Buffer.t ->
              ('b, Buffer.t, unit, 'a) format4 -> 'b;;
(** Same as [bprintf], but instead of returning immediately,
   passes the buffer to its first argument at the end of printing. *)

(**/**)

(* For system use only.  Don't call directly. *)

module CamlinternalPr : sig

  module Sformat : sig
    type index;;

    val index_of_int : int -> index;;
    external int_of_index : index -> int = "%identity";;
    external unsafe_index_of_int : int -> index = "%identity";;

    val succ_index : index -> index;;

    val sub : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> index -> int -> string;;
    val to_string : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string;;
    external length : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int
      = "%string_length";;
    external get : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char
      = "%string_safe_get";;
    external unsafe_to_string : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string
      = "%identity";;
    external unsafe_get : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char
      = "%string_unsafe_get";;

  end;;

  module Tformat : sig

    type ac = {
      mutable ac_rglr : int;
      mutable ac_skip : int;
      mutable ac_rdrs : int;
    };;

    val ac_of_format : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> ac;;

    val sub_format :
        (('a, 'b, 'c, 'd, 'e, 'f) format6 -> int) ->
        (('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char -> int) ->
        char -> ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> int

    val summarize_format_type : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string

    val scan_format : ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
        'g array ->
        Sformat.index ->
        int ->
        (Sformat.index -> string -> int -> 'h) ->
        (Sformat.index -> 'i -> 'j -> int -> 'h) ->
        (Sformat.index -> 'k -> int -> 'h) ->
        (Sformat.index -> int -> 'h) ->
        (Sformat.index -> ('l, 'm, 'n, 'o, 'p, 'q) format6 -> int -> 'h) -> 'h

    val kapr :
        (('a, 'b, 'c, 'd, 'e, 'f) format6 -> Obj.t array -> 'g) ->
        ('a, 'b, 'c, 'd, 'e, 'f) format6 -> 'g
  end;;

end;;

