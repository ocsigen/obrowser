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
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: pervasives.mli,v 1.104.2.1 2006/02/01 14:32:00 doligez Exp $ *)

(** The initially opened module.

   This module provides the basic operations over the built-in types
   (numbers, booleans, strings, exceptions, references, lists, arrays,
   input-output channels, ...)

   This module is automatically opened at the beginning of each compilation.
   All components of this module can therefore be referred by their short
   name, without prefixing them by [Pervasives].
*)

(** {6 Exceptions} *)

external raise : exn -> 'a = "%raise"
(** Raise the given exception value *)

val invalid_arg : string -> 'a
(** Raise exception [Invalid_argument] with the given string. *)

val failwith : string -> 'a
(** Raise exception [Failure] with the given string. *)

exception Exit
(** The [Exit] exception is not raised by any library function.  It is
    provided for use in your programs.*)


(** {6 Comparisons} *)


external ( = ) : 'a -> 'a -> bool = "%equal"
(** [e1 = e2] tests for structural equality of [e1] and [e2].
   Mutable structures (e.g. references and arrays) are equal
   if and only if their current contents are structurally equal,
   even if the two mutable objects are not the same physical object.
   Equality between functional values raises [Invalid_argument].
   Equality between cyclic data structures does not terminate. *)

external ( <> ) : 'a -> 'a -> bool = "%notequal"
(** Negation of {!Pervasives.(=)}. *)

external ( < ) : 'a -> 'a -> bool = "%lessthan"
(** See {!Pervasives.(>=)}. *)

external ( > ) : 'a -> 'a -> bool = "%greaterthan"
(** See {!Pervasives.(>=)}. *)

external ( <= ) : 'a -> 'a -> bool = "%lessequal"
(** See {!Pervasives.(>=)}. *)

external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
(** Structural ordering functions. These functions coincide with
   the usual orderings over integers, characters, strings
   and floating-point numbers, and extend them to a
   total ordering over all types.
   The ordering is compatible with [(=)]. As in the case
   of [(=)], mutable structures are compared by contents.
   Comparison between functional values raises [Invalid_argument].
   Comparison between cyclic structures does not terminate. *)

external compare : 'a -> 'a -> int = "%compare"
(** [compare x y] returns [0] if [x] is equal to [y],
   a negative integer if [x] is less than [y], and a positive integer
   if [x] is greater than [y].  The ordering implemented by [compare]
   is compatible with the comparison predicates [=], [<] and [>]
   defined above,  with one difference on the treatment of the float value
   {!Pervasives.nan}.  Namely, the comparison predicates treat [nan]
   as different from any other float value, including itself;
   while [compare] treats [nan] as equal to itself and less than any
   other float value.  This treatment of [nan] ensures that [compare]
   defines a total ordering relation.

   [compare] applied to functional values may raise [Invalid_argument].
   [compare] applied to cyclic structures may not terminate.

   The [compare] function can be used as the comparison function
   required by the {!Set.Make} and {!Map.Make} functors, as well as
   the {!List.sort} and {!Array.sort} functions. *)

val min : 'a -> 'a -> 'a
(** Return the smaller of the two arguments. *)

val max : 'a -> 'a -> 'a
(** Return the greater of the two arguments. *)

external ( == ) : 'a -> 'a -> bool = "%eq"
(** [e1 == e2] tests for physical equality of [e1] and [e2].
   On integers and characters, physical equality is identical to structural
   equality. On mutable structures, [e1 == e2] is true if and only if
   physical modification of [e1] also affects [e2].
   On non-mutable structures, the behavior of [(==)] is
   implementation-dependent; however, it is guaranteed that
   [e1 == e2] implies [compare e1 e2 = 0]. *)

external ( != ) : 'a -> 'a -> bool = "%noteq"
(** Negation of {!Pervasives.(==)}. *)


(** {6 Boolean operations} *)


external not : bool -> bool = "%boolnot"
(** The boolean negation. *)

external ( && ) : bool -> bool -> bool = "%sequand"
(** The boolean ``and''. Evaluation is sequential, left-to-right:
   in [e1 && e2], [e1] is evaluated first, and if it returns [false],
   [e2] is not evaluated at all. *)

external ( & ) : bool -> bool -> bool = "%sequand"
(** @deprecated {!Pervasives.(&&)} should be used instead. *)

external ( || ) : bool -> bool -> bool = "%sequor"
(** The boolean ``or''. Evaluation is sequential, left-to-right:
   in [e1 || e2], [e1] is evaluated first, and if it returns [true],
   [e2] is not evaluated at all. *)

external ( or ) : bool -> bool -> bool = "%sequor"
(** @deprecated {!Pervasives.(||)} should be used instead.*)


(** {6 Integer arithmetic} *)

(** Integers are 31 bits wide (or 63 bits on 64-bit processors).
   All operations are taken modulo 2{^31} (or 2{^63}).
   They do not fail on overflow. *)

external ( ~- ) : int -> int = "%negint"
(** Unary negation. You can also write [-e] instead of [~-e]. *)

external succ : int -> int = "%succint"
(** [succ x] is [x+1]. *)

external pred : int -> int = "%predint"
(** [pred x] is [x-1]. *)

external ( + ) : int -> int -> int = "%addint"
(** Integer addition. *)

external ( - ) : int -> int -> int = "%subint"
(** Integer subtraction. *)

external ( * ) : int -> int -> int = "%mulint"
(** Integer multiplication. *)

external ( / ) : int -> int -> int = "%divint"
(** Integer division.
   Raise [Division_by_zero] if the second argument is 0.
   Integer division rounds the real quotient of its arguments towards zero.
   More precisely, if [x >= 0] and [y > 0], [x / y] is the greatest integer
   less than or equal to the real quotient of [x] by [y].  Moreover,
   [(-x) / y = x / (-y) = -(x / y)].  *)

external ( mod ) : int -> int -> int = "%modint"
(** Integer remainder.  If [y] is not zero, the result
   of [x mod y] satisfies the following properties:
   [x = (x / y) * y + x mod y] and
   [abs(x mod y) <= abs(y)-1].
   If [y = 0], [x mod y] raises [Division_by_zero].
   Notice that [x mod y] is nonpositive if and only if [x < 0].
   Raise [Division_by_zero] if [y] is zero. *)

val abs : int -> int
(** Return the absolute value of the argument.  Note that this may be
  negative if the argument is [min_int]. *)

val max_int : int
(** The greatest representable integer. *)

val min_int : int
(** The smallest representable integer. *)



(** {7 Bitwise operations} *)


external ( land ) : int -> int -> int = "%andint"
(** Bitwise logical and. *)

external ( lor ) : int -> int -> int = "%orint"
(** Bitwise logical or. *)

external ( lxor ) : int -> int -> int = "%xorint"
(** Bitwise logical exclusive or. *)

val lnot : int -> int
(** Bitwise logical negation. *)

external ( lsl ) : int -> int -> int = "%lslint"
(** [n lsl m] shifts [n] to the left by [m] bits.
   The result is unspecified if [m < 0] or [m >= bitsize],
   where [bitsize] is [32] on a 32-bit platform and
   [64] on a 64-bit platform. *)

external ( lsr ) : int -> int -> int = "%lsrint"
(** [n lsr m] shifts [n] to the right by [m] bits.
   This is a logical shift: zeroes are inserted regardless of
   the sign of [n].
   The result is unspecified if [m < 0] or [m >= bitsize]. *)

external ( asr ) : int -> int -> int = "%asrint"
(** [n asr m] shifts [n] to the right by [m] bits.
   This is an arithmetic shift: the sign bit of [n] is replicated.
   The result is unspecified if [m < 0] or [m >= bitsize]. *)


(** {6 Floating-point arithmetic}

   Caml's floating-point numbers follow the
   IEEE 754 standard, using double precision (64 bits) numbers.
   Floating-point operations never raise an exception on overflow,
   underflow, division by zero, etc.  Instead, special IEEE numbers
   are returned as appropriate, such as [infinity] for [1.0 /. 0.0],
   [neg_infinity] for [-1.0 /. 0.0], and [nan] (``not a number'')
   for [0.0 /. 0.0].  These special numbers then propagate through
   floating-point computations as expected: for instance,
   [1.0 /. infinity] is [0.0], and any operation with [nan] as
   argument returns [nan] as result.
*)

external ( ~-. ) : float -> float = "%negfloat"
(** Unary negation. You can also write [-.e] instead of [~-.e]. *)

external ( +. ) : float -> float -> float = "%addfloat"
(** Floating-point addition *)

external ( -. ) : float -> float -> float = "%subfloat"
(** Floating-point subtraction *)

external ( *. ) : float -> float -> float = "%mulfloat"
(** Floating-point multiplication *)

external ( /. ) : float -> float -> float = "%divfloat"
(** Floating-point division. *)

external ( ** ) : float -> float -> float = "caml_power_float" "pow" "float"
(** Exponentiation *)

external sqrt : float -> float = "caml_sqrt_float" "sqrt" "float"
(** Square root *)

external exp : float -> float = "caml_exp_float" "exp" "float"
(** Exponential. *)

external log : float -> float = "caml_log_float" "log" "float"
(** Natural logarithm. *)

external log10 : float -> float = "caml_log10_float" "log10" "float"
(** Base 10 logarithm. *)

external cos : float -> float = "caml_cos_float" "cos" "float"
(** See {!Pervasives.atan2}. *)

external sin : float -> float = "caml_sin_float" "sin" "float"
(** See {!Pervasives.atan2}. *)

external tan : float -> float = "caml_tan_float" "tan" "float"
(** See {!Pervasives.atan2}. *)

external acos : float -> float = "caml_acos_float" "acos" "float"
(** See {!Pervasives.atan2}. *)

external asin : float -> float = "caml_asin_float" "asin" "float"
(** See {!Pervasives.atan2}. *)

external atan : float -> float = "caml_atan_float" "atan" "float"
(** See {!Pervasives.atan2}. *)

external atan2 : float -> float -> float = "caml_atan2_float" "atan2" "float"
(** The usual trigonometric functions. *)

external cosh : float -> float = "caml_cosh_float" "cosh" "float"
(** See {!Pervasives.tanh}. *)

external sinh : float -> float = "caml_sinh_float" "sinh" "float"
(** See {!Pervasives.tanh}. *)

external tanh : float -> float = "caml_tanh_float" "tanh" "float"
(** The usual hyperbolic trigonometric functions. *)

external ceil : float -> float = "caml_ceil_float" "ceil" "float"
(** See {!Pervasives.floor}. *)

external floor : float -> float = "caml_floor_float" "floor" "float"
(** Round the given float to an integer value.
   [floor f] returns the greatest integer value less than or
   equal to [f].
   [ceil f] returns the least integer value greater than or
   equal to [f]. *)

external abs_float : float -> float = "%absfloat"
(** Return the absolute value of the argument. *)

external mod_float : float -> float -> float = "caml_fmod_float" "fmod" "float"
(** [mod_float a b] returns the remainder of [a] with respect to
   [b].  The returned value is [a -. n *. b], where [n]
   is the quotient [a /. b] rounded towards zero to an integer. *)

external frexp : float -> float * int = "caml_frexp_float"
(** [frexp f] returns the pair of the significant
   and the exponent of [f].  When [f] is zero, the
   significant [x] and the exponent [n] of [f] are equal to
   zero.  When [f] is non-zero, they are defined by
   [f = x *. 2 ** n] and [0.5 <= x < 1.0]. *)

external ldexp : float -> int -> float = "caml_ldexp_float"
(** [ldexp x n] returns [x *. 2 ** n]. *)

external modf : float -> float * float = "caml_modf_float"
(** [modf f] returns the pair of the fractional and integral
   part of [f]. *)

external float : int -> float = "%floatofint"
(** Same as {!Pervasives.float_of_int}. *)

external float_of_int : int -> float = "%floatofint"
(** Convert an integer to floating-point. *)

external truncate : float -> int = "%intoffloat"
(** Same as {!Pervasives.int_of_float}. *)

external int_of_float : float -> int = "%intoffloat"
(** Truncate the given floating-point number to an integer.
   The result is unspecified if it falls outside the
   range of representable integers. *)

val infinity : float
(** Positive infinity. *)

val neg_infinity : float
(** Negative infinity. *)

val nan : float
(** A special floating-point value denoting the result of an
   undefined operation such as [0.0 /. 0.0].  Stands for
   ``not a number''.  Any floating-point operation with [nan] as
   argument returns [nan] as result.  As for floating-point comparisons,
   [=], [<], [<=], [>] and [>=] return [false] and [<>] returns [true]
   if one or both of their arguments is [nan]. *)

val max_float : float
(** The largest positive finite value of type [float]. *)

val min_float : float
(** The smallest positive, non-zero, non-denormalized value of type [float]. *)

val epsilon_float : float
(** The smallest positive float [x] such that [1.0 +. x <> 1.0]. *)

type fpclass =
    FP_normal           (** Normal number, none of the below *)
  | FP_subnormal        (** Number very close to 0.0, has reduced precision *)
  | FP_zero             (** Number is 0.0 or -0.0 *)
  | FP_infinite         (** Number is positive or negative infinity *)
  | FP_nan              (** Not a number: result of an undefined operation *)
(** The five classes of floating-point numbers, as determined by
   the {!Pervasives.classify_float} function. *)

external classify_float : float -> fpclass = "caml_classify_float"
(** Return the class of the given floating-point number:
   normal, subnormal, zero, infinite, or not a number. *)


(** {6 String operations}

   More string operations are provided in module {!String}.
*)

val ( ^ ) : string -> string -> string
(** String concatenation. *)


(** {6 Character operations}

   More character operations are provided in module {!Char}.
*)

external int_of_char : char -> int = "%identity"
(** Return the ASCII code of the argument. *)

val char_of_int : int -> char
(** Return the character with the given ASCII code.
   Raise [Invalid_argument "char_of_int"] if the argument is
   outside the range 0--255. *)


(** {6 Unit operations} *)

external ignore : 'a -> unit = "%ignore"
(** Discard the value of its argument and return [()].
   For instance, [ignore(f x)] discards the result of
   the side-effecting function [f].  It is equivalent to
   [f x; ()], except that the latter may generate a
   compiler warning; writing [ignore(f x)] instead
   avoids the warning. *)


(** {6 String conversion functions} *)

val string_of_bool : bool -> string
(** Return the string representation of a boolean. *)

val bool_of_string : string -> bool
(** Convert the given string to a boolean.
   Raise [Invalid_argument "bool_of_string"] if the string is not
   ["true"] or ["false"]. *)

val string_of_int : int -> string
(** Return the string representation of an integer, in decimal. *)

external int_of_string : string -> int = "caml_int_of_string"
(** Convert the given string to an integer.
   The string is read in decimal (by default) or in hexadecimal (if it
   begins with [0x] or [0X]), octal (if it begins with [0o] or [0O]),
   or binary (if it begins with [0b] or [0B]).
   Raise [Failure "int_of_string"] if the given string is not
   a valid representation of an integer, or if the integer represented
   exceeds the range of integers representable in type [int]. *)

val string_of_float : float -> string
(** Return the string representation of a floating-point number. *)

external float_of_string : string -> float = "caml_float_of_string"
(** Convert the given string to a float.  Raise [Failure "float_of_string"]
   if the given string is not a valid representation of a float. *)



(** {6 Pair operations} *)

external fst : 'a * 'b -> 'a = "%field0"
(** Return the first component of a pair. *)

external snd : 'a * 'b -> 'b = "%field1"
(** Return the second component of a pair. *)


(** {6 List operations}

   More list operations are provided in module {!List}.
*)

val ( @ ) : 'a list -> 'a list -> 'a list
(** List concatenation. *)

(** {6 References} *)

type 'a ref = { mutable contents : 'a }
(** The type of references (mutable indirection cells) containing
   a value of type ['a]. *)

external ref : 'a -> 'a ref = "%makemutable"
(** Return a fresh reference containing the given value. *)

external ( ! ) : 'a ref -> 'a = "%field0"
(** [!r] returns the current contents of reference [r].
   Equivalent to [fun r -> r.contents]. *)

external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
(** [r := a] stores the value of [a] in reference [r].
   Equivalent to [fun r v -> r.contents <- v]. *)

external incr : int ref -> unit = "%incr"
(** Increment the integer contained in the given reference.
   Equivalent to [fun r -> r := succ !r]. *)

external decr : int ref -> unit = "%decr"
(** Decrement the integer contained in the given reference.
   Equivalent to [fun r -> r := pred !r]. *)


(** {6 Operations on format strings} *)

(** See modules {!Printf} and {!Scanf} for more operations on
    format strings. *)
type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6 

type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4
(** Simplified type for format strings, included for backward compatibility
    with earlier releases of Objective Caml.
    ['a] is the type of the parameters of the format,
    ['c] is the result type for the "printf"-style function,
    and ['b] is the type of the first argument given to
    [%a] and [%t] printing functions. *)

val string_of_format : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string
(** Converts a format string into a string. *)

external format_of_string :
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"
(** [format_of_string s] returns a format string read from the string
    literal [s]. *)

val ( ^^ ) :
      ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
      ('f, 'b, 'c, 'e, 'g, 'h) format6 ->
      ('a, 'b, 'c, 'd, 'g, 'h) format6
(** [f1 ^^ f2] catenates formats [f1] and [f2].  The result is a format
  that accepts arguments from [f1], then arguments from [f2]. *)

(** {6 Program termination} *)


val exit : int -> 'a
(** Terminate the process, returning the given status code
   to the operating system: usually 0 to indicate no errors,
   and a small positive integer to indicate failure. 
   All open output channels are flushed with flush_all.
   An implicit [exit 0] is performed each time a program
   terminates normally.  An implicit [exit 2] is performed if the program
   terminates early because of an uncaught exception. *)

val at_exit : (unit -> unit) -> unit
(** Register the given function to be called at program
   termination time. The functions registered with [at_exit]
   will be called when the program executes {!Pervasives.exit},
   or terminates, either normally or because of an uncaught exception.
   The functions are called in ``last in, first out'' order:
   the function most recently added with [at_exit] is called first. *)


(**/**)

(** {6 For system use only, not for the casual user} *)

val valid_float_lexem : string -> string

val do_at_exit : unit -> unit
