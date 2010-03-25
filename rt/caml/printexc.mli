
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

(* $Id: printexc.mli 9335 2009-09-16 13:34:57Z xclerc $ *)

(** Facilities for printing exceptions. *)

val to_string : exn -> string
(** [Printexc.to_string e] returns a string representation of
   the exception [e]. *)

val register_printer : (exn -> string option) -> unit
(** [Printexc.register_printer fn] registers [fn] as an exception printer.
    The printer should return [None] if it does not know how to convert
    the passed exception, and [Some s] with [s] the resulting string if
    it can convert the passed exception.
    When converting an exception into a string, the printers will be invoked
    in the reverse order of their registrations, until a printer returns
    a [Some s] value (if no such printer exists, the runtime will use a
    generic printer). *)
