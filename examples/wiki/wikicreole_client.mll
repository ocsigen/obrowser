{
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
   Parser for Wikicreole
   @author Jérôme Vouillon
   @author Vincent Balat
*)

exception Eof
exception Unrecognized_char

type attribs = (string * string) list

type ('a, 'b, 'c) ext_kind = 
  | Block of 'a
  | A_content of 'b
  | Link_plugin of 'c

type ('flow, 'inline, 'a_content, 'param, 'sp) builder =
  { chars : string -> 'a_content;
    strong_elem : attribs -> 'inline list -> 'a_content;
    em_elem : attribs -> 'inline list -> 'a_content;
    br_elem : attribs -> 'a_content;
    img_elem : attribs -> string -> string -> 'a_content;
    tt_elem : attribs -> 'inline list -> 'a_content;
    monospace_elem : attribs -> 'inline list -> 'a_content;
    underlined_elem : attribs -> 'inline list -> 'a_content;
    linethrough_elem : attribs -> 'inline list -> 'a_content;
    subscripted_elem : attribs -> 'inline list -> 'a_content;
    superscripted_elem : attribs -> 'inline list -> 'a_content;
    nbsp : 'a_content;
    a_elem : attribs -> 'sp -> string -> 'a_content list -> 'inline;
    make_href : 'sp -> string -> string;
    p_elem : attribs -> 'inline list -> 'flow;
    pre_elem : attribs -> string list -> 'flow;
    h1_elem : attribs -> 'inline list -> 'flow;
    h2_elem : attribs -> 'inline list -> 'flow;
    h3_elem : attribs -> 'inline list -> 'flow;
    h4_elem : attribs -> 'inline list -> 'flow;
    h5_elem : attribs -> 'inline list -> 'flow;
    h6_elem : attribs -> 'inline list -> 'flow;
    ul_elem : attribs -> ('inline list * 'flow option * attribs) list -> 'flow;
    ol_elem : attribs -> ('inline list * 'flow option * attribs) list -> 'flow;
    dl_elem : attribs -> (bool * 'inline list * attribs) list -> 'flow;
    hr_elem : attribs -> 'flow;
    table_elem : attribs -> 
      ((bool * attribs * 'inline list) list * attribs) list -> 'flow;
    inline : 'a_content -> 'inline;
    plugin : 
      string ->
       (bool *
          ('param -> (string * string) list -> string option ->
             (('flow, 'a_content, (string * attribs * 'a_content)) ext_kind)));
    plugin_action : 
      string -> int -> int -> 
      'param -> (string * string) list -> string option -> unit;
    error : string -> 'a_content;
  }

type style =
    Bold | Italic | Underlined | Linethrough |
        Monospace | Superscripted | Subscripted

type list_kind = Unordered | Ordered

type ('inline, 'flow) stack =
    Style of style * 'inline list * attribs * ('inline, 'flow) stack
  | Link of string * attribs * ('inline, 'flow) stack
      (* Not that we do not save anything in the case of links, as
         links cannot be nested *)
  | Paragraph of attribs
  | Heading of int * attribs
  | List_item of attribs * ('inline, 'flow) stack
  | List of
      list_kind * ('inline list * 'flow option * attribs) list
      * attribs * ('inline, 'flow) stack
  | Descr_def of attribs * ('inline, 'flow) stack
  | Descr_title of attribs * ('inline, 'flow) stack
  | Descr of attribs * ('inline, 'flow) stack
  | Table of ((bool * attribs * 'inline list) list * attribs) list * attribs
  | Row of (bool * attribs * 'inline list) list * attribs * ('inline, 'flow) stack
  | Entry of bool * attribs * ('inline, 'flow) stack

type ('flow, 'inline, 'a_content, 'param, 'sp) ctx =
  { build : ('flow, 'inline, 'a_content, 'param, 'sp) builder;
    param : 'param;
    sp : 'sp;
    mutable italic : bool;
    mutable bold : bool;
    mutable monospace : bool;
    mutable underlined : bool;
    mutable linethrough : bool;
    mutable subscripted : bool;
    mutable superscripted : bool;
    mutable heading : bool;
    mutable link : bool;
    mutable list_level : int;
    mutable inline_mix : 'inline list;
    mutable link_content : 'a_content list;
    mutable pre_content : string list;
    mutable list : ('inline list * 'flow option * attribs) list;
    mutable descr : (bool * 'inline list * attribs) list;
    mutable flow : 'flow list;
    mutable stack : ('inline, 'flow) stack }

let count c s =
  let n = ref 0 in
  for i = 0 to String.length s - 1 do if s.[i] = c then incr n done;
  !n

let push c v =
  match c.stack with
    Link _ -> c.link_content <- v :: c.link_content
  | _      -> c.inline_mix <- c.build.inline v :: c.inline_mix

let push_string c s = push c (c.build.chars s)

let push_chars c lexbuf = push_string c (Lexing.lexeme lexbuf)

let read_attribs att parse_attribs c lexbuf =
  try
    if att = "@@"
    then match parse_attribs 1 [] [] c lexbuf with
      | [] -> []
      | a::_ -> a
    else []
  with Eof -> [] (*VVV ??? *)

let read_list_attribs att parse_attribs c lexbuf =
  try
    if att = "@@"
    then match parse_attribs 2 [] [] c lexbuf with
      | [] -> ([], [])
      | a::b::_ -> (b, a)
      | [a] -> ([], a)
    else ([], [])
  with Eof -> ([], []) (*VVV ??? *)

let read_table_attribs att parse_attribs c lexbuf =
  try
    if att = "@@"
    then match parse_attribs 3 [] [] c lexbuf with
      | [] -> ([], [], [])
      | a::b::c::_ -> (c, b, a)
      | [a; b] -> ([], b, a)
      | [a] -> ([], [], a)
    else ([], [], [])
  with Eof -> ([], [], []) (*VVV ??? *)

let get_style c style =
  match style with 
    | Bold -> c.bold
    | Italic -> c.italic
    | Monospace -> c.monospace
    | Underlined -> c.underlined
    | Linethrough -> c.linethrough
    | Subscripted -> c.subscripted
    | Superscripted -> c.superscripted

let set_style c style v =
  match style with
    | Bold -> c.bold <- v
    | Italic -> c.italic <- v
    | Monospace -> c.monospace <- v
    | Underlined -> c.underlined <- v
    | Linethrough -> c.linethrough <- v
    | Subscripted -> c.subscripted <- v
    | Superscripted -> c.superscripted <- v

let pop_style c style inline attribs stack =
  let elt =
    match style with
      | Bold   -> c.build.strong_elem attribs
      | Italic -> c.build.em_elem attribs
      | Monospace -> c.build.monospace_elem attribs
      | Underlined -> c.build.underlined_elem attribs
      | Linethrough -> c.build.linethrough_elem attribs
      | Subscripted -> c.build.subscripted_elem attribs
      | Superscripted -> c.build.superscripted_elem attribs
  in
  let inline' = c.inline_mix in
  c.stack <- stack;
  c.inline_mix <- inline;
  push c (elt (List.rev inline'));
  set_style c style false

let style_change c style att parse_attribs lexbuf =
  let atts = read_attribs att parse_attribs c lexbuf in
  if get_style c style then begin
    match c.stack with
      Style (s, inline, attribs, stack) when s = style ->
        pop_style c style inline attribs stack;
    | _ ->
        push_string c "**";
        push_string c att
  end else begin
    c.stack <- Style (style, c.inline_mix, atts, c.stack);
    c.inline_mix <- [];
    set_style c style true
  end

let pop_link c addr attribs stack =
  c.stack <- stack;
  c.inline_mix <-
    c.build.a_elem attribs c.sp addr (List.rev c.link_content) :: c.inline_mix;
  c.link_content <- [];
  c.link <- false

let close_entry c =
  match c.stack with
    Entry (heading, attribs, Row (entries, row_attribs, stack)) ->
      c.stack <- Row ((heading, attribs, List.rev c.inline_mix) :: entries, 
                      row_attribs,
                      stack);
      c.inline_mix <- [];
      true
  | Row _ | Table _ ->
      true
  | _ ->
      false

let close_row c =
  close_entry c &&
  match c.stack with
    Row (entries, row_attribs, Table (rows, table_attribs)) ->
      c.stack <- Table (((List.rev entries, row_attribs) :: rows), 
                        table_attribs);
      true
  | Table _ ->
      true
  | _ ->
      assert false

let close_descr_entry c =
  match c.stack with
    | Descr_def (attribs, stack) ->
        c.stack <- stack;
        c.descr <- (false, List.rev c.inline_mix, attribs) :: c.descr;
        c.inline_mix <- [];
        true
    | Descr_title (attribs, stack) ->
        c.stack <- stack;
        c.descr <- (true, List.rev c.inline_mix, attribs) :: c.descr;
        c.inline_mix <- [];
        true
    | _ ->
        false


let rec end_paragraph c lev =
  match c.stack with
    Style (style, inline, attribs, stack) ->
      pop_style c style inline attribs stack;
      end_paragraph c lev
  | Link (addr, attribs, stack) ->
      pop_link c addr attribs stack;
      end_paragraph c lev
  | Paragraph attribs ->
      if c.inline_mix <> [] then begin
        c.flow <- c.build.p_elem attribs (List.rev c.inline_mix) :: c.flow;
        c.inline_mix <- []
      end;
      c.stack <- Paragraph []
  | Heading (l, attribs) ->
      let f =
        match l with
          | 1 -> c.build.h1_elem
          | 2 -> c.build.h2_elem
          | 3 -> c.build.h3_elem
          | 4 -> c.build.h4_elem
          | 5 -> c.build.h5_elem
          | _ -> c.build.h6_elem
      in
      c.flow <- f attribs (List.rev c.inline_mix) :: c.flow;
      c.inline_mix <- [];
      c.heading <- false;
      c.stack <- Paragraph []
  | List_item (attribs, stack) ->
      c.list <- (List.rev c.inline_mix, None, attribs) :: c.list;
      c.stack <- stack;
      c.inline_mix <- [];
      end_paragraph c lev
  | List (kind, lst, attribs, stack) ->
      if lev < c.list_level then begin
        c.list_level <- c.list_level - 1;
        let elt =
          match kind with
            Unordered -> c.build.ul_elem
          | Ordered   -> c.build.ol_elem
        in
        let cur_lst = elt attribs (List.rev c.list) in
        if c.list_level = 0 then
          c.flow <- cur_lst :: c.flow
        else begin
          match lst with
            (l, None, attribs) :: rem -> 
              c.list <- (l, Some cur_lst, attribs) :: rem;
          | _                -> assert false
        end;
        c.stack <- stack;
        end_paragraph c lev
      end
  | Descr_def (attribs, stack) ->
      c.descr <- (false, List.rev c.inline_mix, attribs) :: c.descr;
      c.stack <- stack;
      c.inline_mix <- [];
      end_paragraph c lev
  | Descr_title (attribs, stack) ->
      c.descr <- (true, List.rev c.inline_mix, attribs) :: c.descr;
      c.stack <- stack;
      c.inline_mix <- [];
      end_paragraph c lev
  | Descr (attribs, stack) ->
      let lst = c.build.dl_elem attribs (List.rev c.descr) in
      c.flow <- lst :: c.flow;
      c.stack <- stack;
      end_paragraph c lev
  | Entry _ ->
      ignore (close_row c);
      end_paragraph c lev
  | Row _ ->
      assert false
  | Table (rows, attribs) ->
      c.flow <- c.build.table_elem attribs (List.rev rows) :: c.flow;
      c.stack <- Paragraph []

let rec correct_kind_rec stack kind n =
  match stack with
    List_item (_, stack) ->
      correct_kind_rec stack kind n
  | List (k, _lst, _, stack) ->
      if n = 0 then k = kind else
      correct_kind_rec stack kind (n - 1)
  | Style (_, _, _, stack) ->
      correct_kind_rec stack kind n
  | Link _ | Heading _ | Paragraph _ | Entry _ | Row _ | Table _
  | Descr _ | Descr_title _ | Descr_def _ ->
      assert false

let correct_kind c kind lev =
  lev = c.list_level + 1
    ||
  (lev <= c.list_level &&
   correct_kind_rec c.stack kind (c.list_level - lev))

let start_list_item c kind lev att parse_attribs lexbuf =
  let correct = correct_kind c kind lev in
  if lev = 1 || correct then begin
    (* If we have an item of a different kind at level 1, we close the
       previous list and start a new one of the right kind *)
    end_paragraph c (if correct then lev else 0);
    let (list_attribs, item_attribs) =
      read_list_attribs att parse_attribs c lexbuf
    in
    if lev = c.list_level then begin
      c.stack <- List_item (item_attribs, c.stack)
    end else (* if lev = c.list_level + 1 then *) begin
      c.list_level <- lev;
      c.stack <- List_item (item_attribs,
                            List (kind, c.list, list_attribs, c.stack));
      c.list <- []
    end;
    true
  end else
    false

let start_table_row c heading (table_attribs, row_attribs, entry_attribs) =
  if not (close_row c) then begin
    end_paragraph c 0;
    c.stack <- Table ([], table_attribs)
  end;
  c.stack <- Entry (heading, 
                    entry_attribs, 
                    Row ([], row_attribs, c.stack))

let build_extension lexbuf start name ext_info args c content =
    let args = List.rev args in
    c.build.plugin_action name start (Lexing.lexeme_end lexbuf)
      c.param args content;
    ext_info c.param args content

}

let line_break = '\n' | '\r' | "\r\n"
let white_space = [ ' ' '\t' ]
(* XXX Should we consider form feed and zero-width space as white
   spaces as well ? *)

let not_line_break = [^ '\n' '\r']
let reserved_chars = [ '*' '/' '\\' '=' '[' ']' '{' '~' '|' 'h' 'f' '<' '-' '#' '_' '^' ',' ';' ':' ]
let punctuation = [ ',' '.' '?' '!' ':' ';' '"' '\'' ]

let first_char = (not_line_break # ['~' '|']) | ('=' +)
let next_chars = not_line_break # reserved_chars


rule parse_bol c =
  parse
    line_break {
      end_paragraph c 0;
      parse_bol c lexbuf
    }
  | white_space * ("=" | "==" | "===" | "====" | "=====" | "======") 
      (("@@" ?) as att) {
      end_paragraph c 0;
      assert (match c.stack with Paragraph _ -> true | _ -> false);
      let l = count '=' (Lexing.lexeme lexbuf) in
	Js.alert ("OLOL " ^ string_of_int l) ;
      c.stack <- Heading (l, read_attribs att parse_attribs c lexbuf);
      c.heading <- true;
      parse_rem c lexbuf
    }
  | white_space * "*" + (("@@" ?) as att) {
      let lev = count '*' (Lexing.lexeme lexbuf) in
      if not (start_list_item c Unordered lev att parse_attribs lexbuf) 
      then begin
        let s = Lexing.lexeme lexbuf in
        let l = String.index s '*' in
        if l > 0 then push_string c (String.sub s 0 l);
        for i = 1 to lev / 2 - 1 do
          style_change c Bold "" parse_attribs lexbuf
        done;
        if lev land 1 = 1 
        then begin 
          style_change c Bold "" parse_attribs lexbuf;
          push_string c "*";
          push_string c att;
        end
        else
          style_change c Bold att parse_attribs lexbuf
      end;
      parse_rem c lexbuf
    }
  | white_space * "#" + (("@@" ?) as att) {
      let lev = count '#' (Lexing.lexeme lexbuf) in
      if not (start_list_item c Ordered lev att parse_attribs lexbuf)
      then begin
        let s = Lexing.lexeme lexbuf in
        let l = String.index s '#' in
        if l > 0 then push_string c (String.sub s 0 l);
        for i = 1 to lev / 2 - 1 do
          style_change c Monospace "" parse_attribs lexbuf
        done;
        if lev land 1 = 1 
        then begin 
          style_change c Monospace "" parse_attribs lexbuf;
          push_string c "#";
          push_string c att;
        end
        else
          style_change c Bold att parse_attribs lexbuf
      end;
      parse_rem c lexbuf
    }
  | white_space * ";" (("@@" ?) as att) {
      let (list_attribs, item_attribs) =
        read_list_attribs att parse_attribs c lexbuf
      in
      if close_descr_entry c
      then c.stack <- Descr_title (item_attribs, c.stack)
      else begin
        end_paragraph c 0;
        c.stack <- Descr_title (item_attribs, Descr (list_attribs, c.stack));
        c.descr <- []
      end;      
      parse_rem c lexbuf
    }
  | white_space * ":" (("@@" ?) as att) {
      let (list_attribs, item_attribs) =
        read_list_attribs att parse_attribs c lexbuf
      in
      if close_descr_entry c 
      then c.stack <- Descr_def (item_attribs, c.stack)
      else begin
        end_paragraph c 0;
        c.stack <- Descr_def (item_attribs, Descr (list_attribs, c.stack));
        c.descr <- []
      end;
      parse_rem c lexbuf
    }
  | white_space * "----" (("@@" ?) as att) white_space * (line_break | eof) {
      end_paragraph c 0;
      c.flow <- c.build.hr_elem 
        (read_attribs att parse_attribs c lexbuf) :: c.flow;
      parse_bol c lexbuf
    }
  | white_space * "{{{" (("@@" ?) as att) (line_break | eof) {
      end_paragraph c 0;
      parse_nowiki c (read_attribs att parse_attribs c lexbuf) lexbuf
    }
  | white_space * "|" (("@@" ?) as att) {
      start_table_row c false 
        (read_table_attribs att parse_attribs c lexbuf);
      parse_rem c lexbuf
    }
  | white_space * "|=" (("@@" ?) as att) {
      start_table_row c true 
        (read_table_attribs att parse_attribs c lexbuf);
      parse_rem c lexbuf
    }
  | white_space * (("@@" ?) as att) {
      let attribs = read_attribs att parse_attribs c lexbuf in
      if attribs <> []
      then
	(match c.stack with
           | Paragraph _ -> c.stack <- Paragraph attribs
           | _ -> ());
      parse_rem c lexbuf
    }
  | "" {
      parse_rem c lexbuf
    }

and parse_rem c =
  parse
    line_break {
      (* Headings are single lines *)
      if c.heading then
        end_paragraph c 0
      else
        push_chars c lexbuf;
      parse_bol c lexbuf
    }
  | "**" (("@@" ?) as att) {
      style_change c Bold att parse_attribs lexbuf;
      parse_rem c lexbuf
    }
  | "//" (("@@" ?) as att) {
      style_change c Italic att parse_attribs lexbuf;
      parse_rem c lexbuf
    }
  | "##" (("@@" ?) as att) {
      style_change c Monospace att parse_attribs lexbuf;
      parse_rem c lexbuf
    }
  | "^^" (("@@" ?) as att) {
      style_change c Superscripted att parse_attribs lexbuf;
      parse_rem c lexbuf
    }
  | ",," (("@@" ?) as att) {
      style_change c Subscripted att parse_attribs lexbuf;
      parse_rem c lexbuf
    }
  | "__" (("@@" ?) as att) {
      style_change c Underlined att parse_attribs lexbuf;
      parse_rem c lexbuf
    }
  | "--" (("@@" ?) as att) {
      style_change c Linethrough att parse_attribs lexbuf;
      parse_rem c lexbuf
    }
  | "=" + white_space * (line_break | eof) {
      if c.heading then
        end_paragraph c 0
      else
        push_chars c lexbuf;
      parse_bol c lexbuf
    }
  | "[[" (("@@" ?) as att) 
      { parse_link c (read_attribs att parse_attribs c lexbuf) lexbuf }
  | "]]" {
      begin match c.stack with
        Link (addr, attribs, stack) ->
          pop_link c addr attribs stack
      | _ ->
          push_chars c lexbuf
      end;
      parse_rem c lexbuf
    }
  | ("http:" | "ftp:") (not_line_break # white_space) *
    (not_line_break # white_space # punctuation) {
      if c.link then
        push_chars c lexbuf
      else
        let addr = Lexing.lexeme lexbuf in
        c.inline_mix <-
          c.build.a_elem [] c.sp addr [c.build.chars addr] :: c.inline_mix;
      parse_rem c lexbuf
  }
  | "\\\\" (("@@" ?) as att) {
      push c (c.build.br_elem (read_attribs att parse_attribs c lexbuf));
      parse_rem c lexbuf
    }
  | "{{" (("@@" ?) as att)
      { parse_image c (read_attribs att parse_attribs c lexbuf) lexbuf }
  | "<<" ((not_line_break # white_space) # ['|' '>']) * {
      let s = Lexing.lexeme lexbuf in
      let l = String.length s in
      let name = String.sub s 2 (l - 2) in
      let start = Lexing.lexeme_start lexbuf in
      let (wiki_content, ext_info) = c.build.plugin name in
      let content, args = 
        parse_extension start name wiki_content [] c lexbuf
      in
      match build_extension lexbuf start name ext_info args c content with
      | A_content i -> 
          push c i;
          parse_rem c lexbuf
      | Link_plugin (addr, attribs, content) ->
          c.link_content <- [ content ];
          pop_link c addr attribs c.stack;
          parse_rem c lexbuf
      | Block b ->
          end_paragraph c 0;
          c.flow <- b :: c.flow;
          parse_bol c lexbuf
    }
  | "{{{" (("@@" ?) as att)
      { parse_tt c (read_attribs att parse_attribs c lexbuf) lexbuf }
  | '~' (not_line_break # white_space) {
      let s = Lexing.lexeme lexbuf in
      (* It amounts to the same to quote a UTF-8 char or its first byte *)
      push_string c (String.sub s 1 1);
      parse_rem c lexbuf
    }
  | "~ " {
      push c c.build.nbsp;
      parse_rem c lexbuf
    }
  | '|' white_space* (line_break | eof) {
      if not (close_row c) then
        push_chars c lexbuf;
      parse_bol c lexbuf
    }
  | '|' (("@@" ?) as att) {
      if close_entry c then
        c.stack <- Entry (false, 
                          read_attribs att parse_attribs c lexbuf, 
                          c.stack)
      else
        push_chars c lexbuf;
      parse_rem c lexbuf
    }
  | "|=" (("@@" ?) as att) {
      if close_entry c then
        c.stack <- Entry (true, 
                          read_attribs att parse_attribs c lexbuf, 
                          c.stack)
      else
        push_chars c lexbuf;
      parse_rem c lexbuf
    }
  | first_char next_chars * | '~' {
      push_chars c lexbuf;
      parse_rem c lexbuf
    }
  | _ {
     Js.alert 
       ("Wikicreole: Unrecognized char "^(Lexing.lexeme lexbuf)^".");
     raise Unrecognized_char
  }
  | eof {
      end_paragraph c 0
    }

and parse_link c attribs =
    parse 
        (']' ? (not_line_break # [ ']' '|' ])) * "]]" {
      if c.link then begin
        push_string c "[["; (*VVV We loose attributes *)
        push_chars c lexbuf
      end
      else
        let s = Lexing.lexeme lexbuf in
        let addr = 
          c.build.make_href c.sp (String.sub s 0 (String.length s - 2)) 
        in
        c.inline_mix <-
         c.build.a_elem attribs c.sp addr [c.build.chars addr] :: c.inline_mix;
      parse_rem c lexbuf
  }
  | (']' ? (not_line_break # [ ']' '|' ])) * "|" {
      if c.link then begin
        push_string c "[["; (*VVV We loose attributes *)
        push_chars c lexbuf
      end
      else begin
        let s = Lexing.lexeme lexbuf in
        let addr = c.build.make_href c.sp (String.sub s 0 (String.length s - 1)) in
        c.stack <- Link (addr, attribs, c.stack);
        c.link <- true
      end;
      parse_rem c lexbuf
  }
  | "" {
      push_string c "[["; (*VVV We loose attributes *)
      parse_rem c lexbuf
    }

and parse_image c attribs =
    parse
     (not_line_break # ['|' '{']) (not_line_break # '|') * '|'
         ('}' ? (not_line_break # '}')) * "}}" {
      let s = Lexing.lexeme lexbuf in
      let i = String.index s '|' in
      let url = c.build.make_href c.sp (String.sub s 0 i) in
      let alt = String.sub s (i + 1) (String.length s - i - 3) in
      push c (c.build.img_elem attribs url alt);
      parse_rem c lexbuf
    }
  | "" {
      push_string c "{{"; (*VVV We loose attributes *)
      parse_rem c lexbuf
    }

and parse_tt c attribs =
    parse
        ('}' ? '}' ? (not_line_break # '}')) * '}' * "}}}" {
      let s = Lexing.lexeme lexbuf in
      let txt = String.sub s 0 (String.length s - 3) in
      push c (c.build.tt_elem attribs [c.build.inline (c.build.chars txt)]);
      parse_rem c lexbuf
    }
  | "" {
      push_string c "{{{"; (*VVV We loose attributes *)
      parse_rem c lexbuf
    }

and parse_nowiki c attribs =
  parse
    white_space + "}}}" (line_break | eof) {
      let s = Lexing.lexeme lexbuf in
      c.pre_content <- String.sub s 1 (String.length s - 1) :: c.pre_content;
      parse_nowiki c attribs lexbuf
    }
  | ("}}}" (line_break | eof)) | eof {
      c.flow <- c.build.pre_elem attribs (List.rev c.pre_content) :: c.flow;
      c.pre_content <- [];
      parse_bol c lexbuf
    }
  | not_line_break * (line_break | eof) {
      c.pre_content <- Lexing.lexeme lexbuf :: c.pre_content;
      parse_nowiki c attribs lexbuf
    }

and parse_extension start name wiki_content args c =
    parse
    | '|' {
        if wiki_content
        then ((parse_extension_content_wiki start 0 false name "" c lexbuf),
              args)
        else ((parse_extension_content_nowiki start name "" c lexbuf), args)
      }
    | (">>" | eof) {
        (None, args)
      }
    |  ';'* | (white_space *) | (line_break *) {
        parse_extension start name wiki_content args c lexbuf
      }
    | (not_line_break # white_space # '=' # '>') * '=' 
        ((white_space | line_break) *) (('\'' | '"') as quote) {
        let s = Lexing.lexeme lexbuf in
        let i = String.index s '=' in
        let arg_name = String.sub s 0 i in
        let arg_value = parse_arg_value quote "" c lexbuf in
        parse_extension start name wiki_content
          ((arg_name, arg_value)::args) c lexbuf
      }
    | _ {
        ignore (if wiki_content
                then ((parse_extension_content_wiki 
                         start 0 false name "" c lexbuf), args)
                else ((parse_extension_content_nowiki
                         start name "" c lexbuf), args));
        (Some ("Syntax error in extension "^name), args)
      }

and parse_extension_content_wiki start lev nowiki name beg c =
    parse
        '~' (('<' | '>' | '~') as ch) {
          parse_extension_content_wiki
            start lev nowiki name (beg^"~"^(String.make 1 ch)) c lexbuf
        }
      | "<<" {
          if nowiki
          then
            parse_extension_content_wiki
              start lev nowiki name (beg^"<<") c lexbuf
          else
            parse_extension_content_wiki
              start (lev+1) nowiki name (beg^"<<") c lexbuf
        }
      | "{{{" {
          parse_extension_content_wiki
            start lev true name (beg^"{{{") c lexbuf
        }
      | "}}}" {
(*VVV Warning: not quotable! *)
          parse_extension_content_wiki
            start lev false name (beg^"}}}") c lexbuf
        }
      | (">>" | eof) {
          if nowiki
          then 
            parse_extension_content_wiki
              start lev nowiki name (beg^">>") c lexbuf
          else
            if lev>0
            then
              parse_extension_content_wiki
                start (lev-1) nowiki name (beg^">>") c lexbuf
            else Some beg
        }
      | [^ '~' '>' '<' '{' '}' ]+ {
          let s = Lexing.lexeme lexbuf in
          parse_extension_content_wiki start lev nowiki name (beg^s) c lexbuf
        }
      | [ '>' '<' '~' '{' '}' ] {
          let s = Lexing.lexeme lexbuf in
          parse_extension_content_wiki start lev nowiki name (beg^s) c lexbuf
        }
      | _ {
          Js.alert
            ("Wikicreole: Unrecognized char in extension "^
               (Lexing.lexeme lexbuf)^".");
          raise Unrecognized_char
        }

and parse_extension_content_nowiki start name beg c =
    parse
      | ("~>>" | eof) {
          parse_extension_content_nowiki
            start name (beg^">>") c lexbuf
        }
      | (">>" | eof) {
          Some beg
        }
      | [^ '~' '>' ]+ {
          let s = Lexing.lexeme lexbuf in
          parse_extension_content_nowiki start name (beg^s) c lexbuf
        }
      | [ '>' '~' ] {
          let s = Lexing.lexeme lexbuf in
          parse_extension_content_nowiki start name (beg^s) c lexbuf
        }
      | _ {
          Js.alert
            ("Wikicreole: Unrecognized char in extension "^
               (Lexing.lexeme lexbuf)^".");
          raise Unrecognized_char
        }


and parse_arg_value quote beg c =
    parse
      eof | '~' eof {
        raise Eof
      }
    | '~' ('\'' | '"' | '~' as ch) {
        parse_arg_value quote (beg^(String.make 1 ch)) c lexbuf
      }
    | ('\'' | '"' | '~') as ch {
        if ch = quote 
        then beg
        else parse_arg_value quote (beg^(String.make 1 ch)) c lexbuf
      }
    | [^ '~' '\'' '"' ] * {
        let s = Lexing.lexeme lexbuf in
        parse_arg_value quote (beg^s) c lexbuf
      }

and parse_attribs depth args oldargs c =
    parse
     "" {
        args::oldargs
      }
    | ';'* | (white_space *) | (line_break *) {
        parse_attribs depth args oldargs c lexbuf
      }
    | (not_line_break # white_space # '=' # '@') * '=' 
          ((white_space | line_break) *) (('\'' | '"') as quote) {
            let s = Lexing.lexeme lexbuf in
            let i = String.index s '=' in
            let arg_name = String.sub s 0 i in
            let arg_value = parse_arg_value quote "" c lexbuf in
            parse_attribs depth ((arg_name, arg_value)::args) oldargs c lexbuf
          }
    | "@@" { args::oldargs }
    | "@" { 
        if depth > 1
        then parse_attribs (depth - 1) [] (args::oldargs) c lexbuf 
        else args::oldargs 
      }
    | "@@@" { []::args::oldargs }
    | "@@@@" { []::[]::args::oldargs }
    | eof {
        raise Eof
      }


{

let context sp param b =
  { build = b; 
    param = param;
    sp = sp;
    italic = false; 
    bold = false;
    monospace = false;
    underlined = false;
    linethrough = false;
    subscripted = false;
    superscripted = false;
    heading = false; 
    link = false; 
    list_level = 0;
    inline_mix = []; 
    link_content = []; 
    pre_content = []; 
    list = []; 
    descr = []; 
    flow = [];
    stack = Paragraph [] }

let from_lexbuf sp param b lexbuf =
  let c = context sp param b in
  parse_bol c lexbuf;
  List.rev c.flow

let from_string sp param b s = from_lexbuf sp param b (Lexing.from_string s)

}
