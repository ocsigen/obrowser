(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_obrowser
 * Copyright (C) 2010 Vincent Balat and Raphaël Proust
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

let (>>>) x f = f x

let sleep s = Lwt_preemptive.detach Thread.delay s

let yield () = sleep 0.0

let run = Lwt_preemptive.run

(*****************************************************************************)
(* some functions taken from AXO: (what is the best place for them?) *)
let window = JSOO.eval "window"

(* internal only : encode a string according to percent-encoding system *)
let urlencode_string str =
  window >>> JSOO.call_method "escape" [| JSOO.string str |]
         >>> JSOO.as_string

(* decode a string according to percent encoding system *)
let urldecode_string str =
  window >>> JSOO.call_method "unescape" [| JSOO.string str |]
         >>> JSOO.as_string

(* internal only : takes a list of (name,value) and makes it url-friendly *)
let urlencode args =
 String.concat "&"
   (List.map
      (fun (n,v) -> (urlencode_string n) ^ "=" ^ (urlencode_string v))
      args
   )

(* the following encode the whole string, even regular chars.
   Otherwise it does not work with Ocaml's Marshalled data *)
let urlencode_string_ s =
  let hex c =
    Char.chr ((if c < 10 then Char.code '0' else Char.code 'A' - 10) + c)
  in
  let s' = String.make (String.length s * 3) ' ' in
  for i = 0 to String.length s - 1 do
    s'.[i * 3] <- '%' ;
    s'.[i * 3 + 1] <- hex ((Char.code s.[i]) lsr 4) ;
    s'.[i * 3 + 2] <- hex ((Char.code s.[i]) land 0xF)
  done ;
  s'

let urlencode_ args =
 String.concat "&"
   (List.map
      (fun (n,v) -> (urlencode_string_ n) ^ "=" ^ (urlencode_string_ v))
      args
   )

let http_get url args =
  let url = if args = [] then url else url^(urlencode args) in
  Lwt_preemptive.detach Js.http_get_with_status url

let http_post url args =
  Js.http_post url "application/x-www-form-urlencoded" (urlencode_ args)

let http_get_post url get_args post_args =
  Js.http_post
    (url ^ "?" ^ (urlencode get_args))
    "application/x-www-form-urlencode"
    (urlencode_ post_args)

let http_post url args = Lwt_preemptive.detach (http_post url) args

let http_get_post url get_args post_args =
  Lwt_preemptive.detach (http_get_post url get_args) post_args


  

(* old implementation was:

open Lwt

let (>=>) x f = f x

let sleep_ms t =
  let (res, w) = Lwt.task () in
  let f = Lwt.wakeup w (* à rev ! *) in
  let timeout =
    AXOJs.Node.window
    >=> JSOO.call_method "setTimeout" [| (* f ; *) JSOO.int t |]
  in
  Lwt.on_cancel res
    (fun () -> 
       AXOJs.Node.window
       >=> JSOO.call_method "clearTimeout" [| timeout |]
       >=> ignore);
  res
  
let sleep t =
  let t = int_of_float (t *. 1000.) in
  sleep_ms t

let yield () = sleep_ms 0

exception Timeout

let timeout d = sleep d >> Lwt.fail Timeout

let with_timeout d f = Lwt.select [timeout d; Lwt.apply f ()]

(* We do not need a run function *)


*)
