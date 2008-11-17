(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Modified version for O'Browser by Benjamin Canou         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: format.mli,v 1.74 2006/11/17 08:37:07 weis Exp $ *)

(** Pretty printing.

   This module implements a pretty-printing facility to format text
   within ``pretty-printing boxes''. The pretty-printer breaks lines
   at specified break hints, and indents lines according to the box
   structure.

   For a gentle introduction to the basics of pretty-printing using
   [Format], read
   {{:http://caml.inria.fr/resources/doc/guides/format.html}http://caml.inria.fr/resources/doc/guides/format.html}.

   You may consider this module as providing an extension to the
   [printf] facility to provide automatic line breaking. The addition of
   pretty-printing annotations to your regular [printf] formats gives you
   fancy indentation and line breaks.
   Pretty-printing annotations are described below in the documentation of
   the function {!Format.fprintf}.

   You may also use the explicit box management and printing functions
   provided by this module. This style is more basic but more verbose
   than the [fprintf] concise formats.

   For instance, the sequence
   [open_box 0; print_string "x ="; print_space (); print_int 1; close_box ()]
   that prints [x = 1] within a pretty-printing box, can be
   abbreviated as [printf "@[%s@ %i@]" "x =" 1], or even shorter
   [printf "@[x =@ %i@]" 1].

   Rule of thumb for casual users of this library:
 - use simple boxes (as obtained by [open_box 0]);
 - use simple break hints (as obtained by [print_cut ()] that outputs a
   simple break hint, or by [print_space ()] that outputs a space
   indicating a break hint);
 - once a box is opened, display its material with basic printing
   functions (e. g. [print_int] and [print_string]);
 - when the material for a box has been printed, call [close_box ()] to
   close the box;
 - at the end of your routine, evaluate [print_newline ()] to close
   all remaining boxes and flush the pretty-printer.

   The behaviour of pretty-printing commands is unspecified
   if there is no opened pretty-printing box. Each box opened via
   one of the [open_] functions below must be closed using [close_box]
   for proper formatting. Otherwise, some of the material printed in the
   boxes may not be output, or may be formatted incorrectly.

   In case of interactive use, the system closes all opened boxes and
   flushes all pending text (as with the [print_newline] function)
   after each phrase. Each phrase is therefore executed in the initial
   state of the pretty-printer.

   Warning: the material output by the following functions is delayed
   in the pretty-printer queue in order to compute the proper line
   breaking. Hence, you should not mix calls to the printing functions
   of the basic I/O system with calls to the functions of this module:
   this could result in some strange output seemingly unrelated with
   the evaluation order of printing commands.
*)


(** {6 Tags} *)

type tag = string;;

(** Tags are used to decorate printed entities for user's defined
   purposes, e.g. setting font and giving size indications for a
   display device, or marking delimitations of semantics entities
   (e.g. HTML or TeX elements or terminal escape sequences).

   By default, those tags do not influence line breaking calculation:
   the tag ``markers'' are not considered as part of the printing
   material that drives line breaking (in other words, the length of
   those strings is considered as zero for line breaking).

   Thus, tag handling is in some sense transparent to pretty-printing
   and does not interfere with usual pretty-printing. Hence, a single
   pretty printing routine can output both simple ``verbatim''
   material or richer decorated output depending on the treatment of
   tags. By default, tags are not active, hence the output is not
   decorated with tag information.  Once [set_tags] is set to [true],
   the pretty printer engine honors tags and decorates the output
   accordingly.

   When a tag has been opened (or closed), it is both and successively
   ``printed'' and ``marked''. Printing a tag means calling a
   formatter specific function with the name of the tag as argument:
   that ``tag printing'' function can then print any regular material
   to the formatter (so that this material is enqueued as usual in the
   formatter queue for further line-breaking computation). Marking a
   tag means to output an arbitrary string (the ``tag marker''),
   directly into the output device of the formatter. Hence, the
   formatter specific ``tag marking'' function must return the tag
   marker string associated to its tag argument. Being flushed
   directly into the output device of the formatter, tag marker
   strings are not considered as part of the printing material that
   drives line breaking (in other words, the length of the strings
   corresponding to tag markers is considered as zero for line
   breaking). In addition, advanced users may take advantage of
   the specificity of tag markers to be precisely output when the
   pretty printer has already decided where to break the lines, and
   precisely when the queue is flushed into the output device.

   In the spirit of HTML tags, the default tag marking functions
   output tags enclosed in "<" and ">": hence, the opening marker of
   tag [t] is ["<t>"] and the closing marker ["</t>"].

   Default tag printing functions just do nothing.

   Tag marking and tag printing functions are user definable and can
   be set by calling [set_formatter_tag_functions]. *)

(** {6 Changing the meaning of printing tags} *)

type formatter_tag_functions = {
  mark_open_tag : tag -> string;
  mark_close_tag : tag -> string;
  print_open_tag : tag -> unit;
  print_close_tag : tag -> unit;
};;
(** The tag handling functions specific to a formatter:
   [mark] versions are the ``tag marking'' functions that associate a string
   marker to a tag in order for the pretty-printing engine to flush
   those markers as 0 length tokens in the output device of the formatter.
   [print] versions are the ``tag printing'' functions that can perform
   regular printing when a tag is closed or opened. *)


(** {6 Multiple formatted output} *)

type formatter;;
(** Abstract data type corresponding to a pretty-printer (also called a
   formatter) and all its machinery.
   Defining new pretty-printers permits the output of
   material in parallel on several channels.
   Parameters of a pretty-printer are local to this pretty-printer:
   margin, maximum indentation limit, maximum number of boxes
   simultaneously opened, ellipsis, and so on, are specific to
   each pretty-printer and may be fixed independently.
   Given an output channel [oc], a new formatter writing to
   that channel is obtained by calling [formatter_of_out_channel oc].
   Alternatively, the [make_formatter] function allocates a new
   formatter with explicit output and flushing functions
   (convenient to output material to strings for instance). *)

val formatter_of_buffer : Buffer.t -> formatter;;
(** [formatter_of_buffer b] returns a new formatter writing to
   buffer [b]. As usual, the formatter has to be flushed at
   the end of pretty printing, using [pp_print_flush] or
   [pp_print_newline], to display all the pending material. *)

val stdbuf : Buffer.t;;
(** The string buffer in which [str_formatter] writes. *)

val str_formatter : formatter;;
(** A formatter to use with formatting functions below for
   output to the [stdbuf] string buffer.
   [str_formatter] is defined as [formatter_of_buffer stdbuf]. *)

val flush_str_formatter : unit -> string;;
(** Returns the material printed with [str_formatter], flushes
   the formatter and resets the corresponding buffer. *)

val make_formatter :
  (string -> int -> int -> unit) -> (unit -> unit) -> formatter;;
(** [make_formatter out flush] returns a new formatter that
   writes according to the output function [out], and the flushing
   function [flush]. Hence, a formatter to the out channel [oc]
   is returned by [make_formatter (output oc) (fun () -> flush oc)]. *)

(** {6 Basic functions to use with formatters} *)

val pp_open_hbox : formatter -> unit -> unit;;
val pp_open_vbox : formatter -> int -> unit;;
val pp_open_hvbox : formatter -> int -> unit;;
val pp_open_hovbox : formatter -> int -> unit;;
val pp_open_box : formatter -> int -> unit;;
val pp_close_box : formatter -> unit -> unit;;
val pp_open_tag : formatter -> string -> unit;;
val pp_close_tag : formatter -> unit -> unit;;
val pp_print_string : formatter -> string -> unit;;
val pp_print_as : formatter -> int -> string -> unit;;
val pp_print_int : formatter -> int -> unit;;
val pp_print_float : formatter -> float -> unit;;
val pp_print_char : formatter -> char -> unit;;
val pp_print_bool : formatter -> bool -> unit;;
val pp_print_break : formatter -> int -> int -> unit;;
val pp_print_cut : formatter -> unit -> unit;;
val pp_print_space : formatter -> unit -> unit;;
val pp_force_newline : formatter -> unit -> unit;;
val pp_print_flush : formatter -> unit -> unit;;
val pp_print_newline : formatter -> unit -> unit;;
val pp_print_if_newline : formatter -> unit -> unit;;
val pp_open_tbox : formatter -> unit -> unit;;
val pp_close_tbox : formatter -> unit -> unit;;
val pp_print_tbreak : formatter -> int -> int -> unit;;
val pp_set_tab : formatter -> unit -> unit;;
val pp_print_tab : formatter -> unit -> unit;;
val pp_set_tags : formatter -> bool -> unit;;
val pp_set_print_tags : formatter -> bool -> unit;;
val pp_set_mark_tags : formatter -> bool -> unit;;
val pp_get_print_tags : formatter -> unit -> bool;;
val pp_get_mark_tags : formatter -> unit -> bool;;
val pp_set_margin : formatter -> int -> unit;;
val pp_get_margin : formatter -> unit -> int;;
val pp_set_max_indent : formatter -> int -> unit;;
val pp_get_max_indent : formatter -> unit -> int;;
val pp_set_max_boxes : formatter -> int -> unit;;
val pp_get_max_boxes : formatter -> unit -> int;;
val pp_over_max_boxes : formatter -> unit -> bool;;
val pp_set_ellipsis_text : formatter -> string -> unit;;
val pp_get_ellipsis_text : formatter -> unit -> string;;
val pp_set_formatter_output_functions :
  formatter -> (string -> int -> int -> unit) -> (unit -> unit) -> unit;;
val pp_get_formatter_output_functions :
  formatter -> unit -> (string -> int -> int -> unit) * (unit -> unit);;
val pp_set_all_formatter_output_functions :
  formatter -> out:(string -> int -> int -> unit) -> flush:(unit -> unit) ->
  newline:(unit -> unit) -> spaces:(int -> unit) -> unit;;
val pp_get_all_formatter_output_functions :
  formatter -> unit ->
  (string -> int -> int -> unit) * (unit -> unit) * (unit -> unit) *
  (int -> unit);;
val pp_set_formatter_tag_functions :
  formatter -> formatter_tag_functions -> unit;;
val pp_get_formatter_tag_functions :
  formatter -> unit -> formatter_tag_functions;;
(** These functions are the basic ones: usual functions
   operating on the standard formatter are defined via partial
   evaluation of these primitives. For instance,
   [print_string] is equal to [pp_print_string std_formatter]. *)


(** {6 [printf] like functions for pretty-printing.} *)

val sprintf : ('a, unit, string) format -> 'a;;
(** Same as [printf] above, but instead of printing on a formatter,
   returns a string containing the result of formatting the arguments.
   Note that the pretty-printer queue is flushed at the end of each
   call to [sprintf].

   In case of multiple and related calls to [sprintf] to output
   material on a single string, you should consider using [fprintf]
   with a formatter writing to a buffer: flushing the buffer at the
   end of pretty-printing returns the desired string. You can also use
   the predefined formatter [str_formatter] and call
   [flush_str_formatter ()] to get the result. *)

val bprintf : Buffer.t -> ('a, formatter, unit) format -> 'a;;
(** Same as [sprintf] above, but instead of printing on a string,
   writes into the given extensible buffer.
   As for [sprintf], the pretty-printer queue is flushed at the end of each
   call to [bprintf].

   In case of multiple and related calls to [bprintf] to output
   material on the same buffer [b], you should consider using
   [fprintf] with a formatter writing to the buffer [b] (as obtained
   by [formatter_of_buffer b]), otherwise the repeated flushes of the
   pretty-printer queue would result in unexpected and badly formatted
   output. *)

val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b;;
(** Same as [sprintf] above, but instead of returning the string,
   passes it to the first argument. *)

