(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* $Id: mutex.ml,v 1.8 2001/12/07 13:40:22 xleroy Exp $ *)

type t

external create : unit -> t = "caml_js_mutex_create"
external lock : t -> unit = "caml_js_mutex_lock"
external try_lock : t -> bool = "caml_js_mutex_try_lock"
external unlock : t -> unit = "caml_js_mutex_unlock"
