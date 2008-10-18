(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Rocquencourt                                  *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

(* Let's do floating-point evaluation, for a change. *)

module FloatSemantics = struct

  type number =
      float

  let inject =
    float_of_int

  let ( + ) = ( +. )
  let ( - ) = ( -. )
  let ( * ) = ( *. )
  let ( / ) = ( /. )
  let (~- ) = (~-. )

end

(* Let us now specialize our parameterized parser. *)

module FloatParser =
  Parser.Make(FloatSemantics)

(* The rest is as usual. *)

let eval_phrase print prerr phrase =
  let buf = Lexing.from_string phrase in
    try
      (* Run the parser on a single line of input. *)
      print (Printf.sprintf "%f" (FloatParser.main Lexer.token buf))
    with
      | Lexer.Error msg ->
	  prerr (Printf.sprintf "%s" msg)
      | FloatParser.Error ->
	  prerr (Printf.sprintf "At offset %d: syntax error." (Lexing.lexeme_start buf))
    
