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

(* only one preemptive thread is running lwt at a time 
   (but not always the same) *)
let doing_lwt = Mutex.create ()

(* queue of ready to run lwt threads (to be awoken) *)
let queue_mutex = Mutex.create ()
let (queue_add, queue_take) =
 let lwt_queue = Queue.create () in
 ((fun v ->
     Mutex.lock queue_mutex;
     Queue.add v lwt_queue;
     Mutex.unlock queue_mutex),
  (fun () ->
     Mutex.lock queue_mutex;
     let v = Queue.take lwt_queue in
     Mutex.unlock queue_mutex;
     v
     (* in case of exception Empty, I do not unlock the mutex *)
  ))

let do_lwt_queue () =
  let rec aux () = 
    let w = queue_take () in
    Lwt.wakeup w ();
    aux ()
  in
  try aux ()
  with
    | Queue.Empty -> 
        Mutex.unlock doing_lwt;
        Mutex.unlock queue_mutex (* unlocked after doing_lwt *)
(* gargh. I really hate programming with preemptive threads!!!!
   Vive Lwt ! -- Vincent *)
    | Invalid_argument _ -> Mutex.unlock doing_lwt
        (* should never occur if I'm right.
           But if it does, a lwt thread may be delayed
           until someone call do_lwt_queue again. *)

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
       (* I add the result in the queue of ready lwt promises *)
       queue_add w;

       (* if nobody is doing the lwt queue, I will do it *)
       if Mutex.try_lock doing_lwt
       then do_lwt_queue ()

    ) x 
  in
  t

let yield () =
  let t, w = Lwt.wait () in

  (* I add the result in the queue of ready lwt promises *)
  queue_add w;

  t

