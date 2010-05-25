(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_preemptive
 * Copyright (C) 2010 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

val detach : ('a -> 'b) -> 'a -> 'b Lwt.t

val undetach : ('a -> 'b Lwt.t) -> 'a -> unit
  (** When called by a preemptive thread, [undetach f a]
      will ask the main Lwt thread to execute [f a].
      Exceptions are ignored.
  *)

val yield : unit -> unit Lwt.t
  (** [yield ()] is a threads which suspends itself and then resumes
      as soon as possible and terminates.
      It will let the other Lwt threads work,
      but not always the browser. 
      So if you want for example to update the page,
      use [Lwt_obrowser.yield] instead (which is slower, however).
  *)


val run : 'a Lwt.t -> 'a
