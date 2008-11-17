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
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* $Id: thread.ml,v 1.23 2003/03/20 16:23:04 xleroy Exp $ *)

(* User-level threads *)

type t

(* It is mucho important that the primitives that reschedule are called 
   through an ML function call, not directly. That's because when such a
   primitive returns, the bytecode interpreter is only semi-obedient:
   it takes sp from the new thread, but keeps pc from the old thread.
   But that's OK if all calls to rescheduling primitives are immediately
   followed by a RETURN operation, which will restore the correct pc
   from the stack. Furthermore, the RETURNs must all have the same
   frame size, which means that both the primitives and their ML wrappers
   must take exactly one argument. *)

external thread_new : (unit -> unit) -> t = "thread_new"
external thread_yield : unit -> unit = "thread_yield"
external thread_request_reschedule : unit -> unit = "thread_request_reschedule"
external thread_sleep : unit -> unit = "thread_sleep"
external thread_join : t -> unit = "thread_join"
external thread_delay : float -> unit = "thread_delay"
external thread_wakeup : t -> unit = "thread_wakeup"
external thread_self : unit -> t = "thread_self"
external thread_kill : t -> unit = "thread_kill"
external thread_uncaught_exception : exn -> unit = "thread_uncaught_exception"

external id : t -> int = "thread_id"

(* In sleep() below, we rely on the fact that signals are detected
   only at function applications and beginning of loops,
   making all other operations atomic. *)

let yield () = thread_yield()
let sleep () = thread_sleep()
let delay duration = thread_delay duration
let join th = thread_join th
let wakeup pid = thread_wakeup pid
let self () = thread_self()
let kill pid = thread_kill pid
let exit () = thread_kill(thread_self())

(* For Thread.create, make sure the function passed to thread_new
   always terminates by calling Thread.exit. *)

let create fn arg =
  thread_new
    (fun () ->
      try
        fn arg; exit()
      with x ->
        thread_uncaught_exception x;
        exit())

