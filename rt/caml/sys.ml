
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*            Modified version for O'Browser by Benjamin Canou         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: sys.ml,v 1.138.2.24 2006/09/15 09:18:14 doligez Exp $ *)

(* System interface *)

external get_config: unit -> string * int = "caml_sys_get_config"
external get_argv: unit -> string * string array = "caml_sys_get_argv"

let (executable_name, argv) = get_argv()
let (os_type, word_size) = get_config()
let max_array_length = (1 lsl (word_size - 10)) - 1;;
let max_string_length = word_size / 8 * max_array_length - 1;;

external time: unit -> float = "caml_sys_time"

let ocaml_version = "3.09.3/rtjs"
