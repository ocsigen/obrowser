(* Ocsimore
 * Copyright (C) 2008
 * Laboratoire PPS - Université Paris Diderot - CNRS
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
(**
   Pretty print wiki to DOM elements
   @author Vincent Balat
*)

open Js.Html

module W = Wikicreole_client

let create n ?attrs children = 
  let m = create n ?attrs () in
  List.iter (Js.Node.append m) children ;
  m

let parse_common_attribs attribs =
  let atts =
    try
      let c = List.assoc "class" attribs in
      ["class", c]
    with Not_found -> []
  in
  let atts =
    try
      let c = List.assoc  "id" attribs in
      ("id", c)::atts
    with Not_found -> atts
  in
  atts

let builder wiki_id =
  { W.chars = string;
    W.strong_elem = (fun attribs s -> 
           let attribs = parse_common_attribs attribs in
           create "strong" ~attrs:attribs s);
    W.em_elem = (fun attribs s ->
           let attribs = parse_common_attribs attribs in
           create "em" ~attrs:attribs s);
    W.monospace_elem = (fun attribs s ->
           let attribs = parse_common_attribs attribs in
           create "tt" ~attrs:attribs s);
    W.underlined_elem = (fun attribs s -> 
           let attribs = parse_common_attribs attribs in
           create "span" ~attrs:(("class", "underlined")::attribs) s);
    W.linethrough_elem = (fun attribs s ->
           let attribs = parse_common_attribs attribs in
           create "span" ~attrs:(("class", "linethrough")::attribs) s);
    W.subscripted_elem = (fun attribs s -> 
           let attribs = parse_common_attribs attribs in
           create "sub" ~attrs:attribs s);
    W.superscripted_elem = (fun attribs s -> 
           let attribs = parse_common_attribs attribs in
           create "sup" ~attrs:attribs s);
    W.a_elem =
      (fun attribs _sp addr s -> 
           let attribs = parse_common_attribs attribs in
           create "a" ~attrs:(("href", addr)::attribs) s);
    W.make_href =
      (fun _sp s -> s);
    W.br_elem = (fun attribs -> 
           let attribs = parse_common_attribs attribs in
           create "br" ~attrs:attribs []);
    W.img_elem =
      (fun attribs addr alt -> 
           let attribs = parse_common_attribs attribs in
           create "img" ~attrs:(("alt", alt)::("src", addr)::attribs) []);
    W.tt_elem = 
      (fun attribs s ->
           let attribs = parse_common_attribs attribs in
           create "tt" ~attrs:attribs s);
    W.nbsp = string " ";
    W.p_elem = (fun attribs s -> 
           let attribs = parse_common_attribs attribs in
           create "p" ~attrs:attribs s);
    W.pre_elem = (fun attribs s -> 
           let attribs = parse_common_attribs attribs in
           create "pre" ~attrs:attribs (List.map string s));
    W.h1_elem = (fun attribs s -> 
           let attribs = parse_common_attribs attribs in
           create "h1" ~attrs:attribs s);
    W.h2_elem = (fun attribs s -> 
           let attribs = parse_common_attribs attribs in
           create "h2" ~attrs:attribs s);
    W.h3_elem = (fun attribs s -> 
           let attribs = parse_common_attribs attribs in
           create "h3" ~attrs:attribs s);
    W.h4_elem = (fun attribs s -> 
           let attribs = parse_common_attribs attribs in
           create "strong" ~attrs:attribs s);
    W.h5_elem = (fun attribs s -> 
           let attribs = parse_common_attribs attribs in
           create "h5" ~attrs:attribs s);
    W.h6_elem = (fun attribs s -> 
           let attribs = parse_common_attribs attribs in
           create "h6" ~attrs:attribs s);
    W.ul_elem = (fun attribs s -> 
           let attribs = parse_common_attribs attribs in
           string "not implemented");
    W.ol_elem = (fun attribs s -> 
           let attribs = parse_common_attribs attribs in
           string "not implemented");
    W.dl_elem = (fun attribs s -> 
           let attribs = parse_common_attribs attribs in
           string "not implemented");
    W.hr_elem = (fun attribs -> 
           let attribs = parse_common_attribs attribs in
           create "hr" ~attrs:attribs []);
    W.table_elem = (fun attribs s -> 
           let attribs = parse_common_attribs attribs in
           string "not implemented");
    W.inline = (fun x -> x);
    W.plugin =
      (fun name -> 
         (false,
          (fun _ _ _ ->
             Wikicreole_client.A_content 
               (string "not implemented"))));
    W.plugin_action = (fun _ _ _ _ _ _ -> ());
    W.error = (fun s -> create "b" [string s]);
  }


let xml_of_wiki wiki_id bi s = 
  W.from_string () () (builder wiki_id) s

