(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_preemptive for O'Browser
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

let (>>=) = Lwt.bind

(* queue of ready to run lwt threads (to be awoken) *)
let (queue_add, queue_take) =
 let lwt_queue = Queue.create () in
 ((fun v -> Queue.add v lwt_queue),
  (fun () -> Queue.take lwt_queue))

(* only one preemptive thread is running lwt at a time 
   (but not always the same) *)
let queue_mutex = Mutex.create ()
let queue_not_empty = Condition.create ()

let detach f x =
  let res = ref None in
  let t, w = Lwt.wait () in
  let t = (t >>= fun () -> 
             match !res with
               | Some res -> Lwt.return res
               | None -> assert false (* should never occur *))
  in
  let _ = Thread.create 
    (fun x -> 
       res := Some (f x);
       Mutex.lock queue_mutex;
       (* I add the result in the queue of ready lwt promises *)
       queue_add w;

       Condition.signal queue_not_empty;
       Mutex.unlock queue_mutex
    ) x 
  in
  t

let undetach f x =
  let t, w = Lwt.wait () in
(*VVV What if exception? *)
  ignore (t >>= fun () -> f x);
  Mutex.lock queue_mutex;
  queue_add w;
  Condition.signal queue_not_empty;
  Mutex.unlock queue_mutex

let yield () =
  let t, w = Lwt.wait () in

  (* I add the result in the queue of ready lwt promises *)
  queue_add w;
  (* There is no lock because `yield' is called inside aux (in run) (called
   * inside run => already lock) *)
  (* Ther is no signal for the same reason (called inside run => no thread
   * wait-ing on the condition) *)

  t

let rec run t =
  let rec aux () = 
    match Lwt.poll t with
      | Some x ->
          Mutex.unlock queue_mutex; x
      | None ->
          let w = queue_take () in
          Lwt.wakeup w ();
          aux ()
  in
  Mutex.lock queue_mutex;
  try aux () with
    | Queue.Empty -> 
        Condition.wait queue_not_empty queue_mutex;
        Mutex.unlock queue_mutex;
        run t
