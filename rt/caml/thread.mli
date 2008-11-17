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

(* $Id: thread.mli,v 1.28 2004/07/13 12:25:13 xleroy Exp $ *)

(** Lightweight threads. *)

type t
(** The type of thread handles. *)


(** {6 Thread creation and termination} *)

val create : ('a -> 'b) -> 'a -> t
(** [Thread.create funct arg] creates a new thread of control,
   in which the function application [funct arg]
   is executed concurrently with the other threads of the program.
   The application of [Thread.create]
   returns the handle of the newly created thread.
   The new thread terminates when the application [funct arg]
   returns, either normally or by raising an uncaught exception.
   In the latter case, the exception is printed on standard error,
   but not propagated back to the parent thread. Similarly, the
   result of the application [funct arg] is discarded and not
   directly accessible to the parent thread. *)

val thread_uncaught_exception : exn -> unit

val self : unit -> t
(** Return the thread currently executing. *)

external id : t -> int = "thread_id"
(** Return the identifier of the given thread. A thread identifier
   is an integer that identifies uniquely the thread.
   It can be used to build data structures indexed by threads. *)

val exit : unit -> unit
(** Terminate prematurely the currently executing thread. *)

val kill : t -> unit
(** Terminate prematurely the thread whose handle is given.
   This functionality is available only with bytecode-level threads. *)

(** {6 Suspending threads} *)

val delay : float -> unit
(** [delay d] suspends the execution of the calling thread for
   [d] seconds. The other program threads continue to run during
   this time. *)

val join : t -> unit
(** [join th] suspends the execution of the calling thread
   until the thread [th] has terminated. *)

val yield : unit -> unit
(** Re-schedule the calling thread without suspending it.
   This function can be used to give scheduling hints,
   telling the scheduler that now is a good time to
   switch to other threads. *)

(**/**)

(** {6 Synchronization primitives}

   The following primitives provide the basis for implementing 
   synchronization functions between threads. Their direct use is
   discouraged, as they are very low-level and prone to race conditions
   and deadlocks. The modules {!Mutex}, {!Condition} and {!Event}
   provide higher-level synchronization primitives. *)

val sleep : unit -> unit
(** Suspend the calling thread until another thread reactivates it
   using {!Thread.wakeup}. Just before suspending the thread,
   {!Thread.critical_section} is reset to [false]. Resetting
   {!Thread.critical_section} and suspending the calling thread is an
   atomic operation. *)

val wakeup : t -> unit
(** Reactivate the given thread. After the call to [wakeup],
   the suspended thread will resume execution at some future time. *)

