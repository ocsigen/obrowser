open JSOO

type position = [ `ABSOLUTE | `FIXED | `RELATIVE ]

type dim = [ `PX of int | `EX of int | `EM of int | `PCT of int ]
let px x = `PX x
let ex x = `EX x
let em x = `EM x
let pct x = `PCT x

type color = string

class style obj =
object
  val sty = obj >>> get "style"

  (* generic methods *)

  method set_dim (n : string) (v : dim) : unit =
    sty >>> set n
      (match v with
	 | `PX v -> string (string_of_int v ^ "px")
	 | `EX v -> string (string_of_int v ^ "px")
	 | `EM v -> string (string_of_int v ^ "px")
	 | `PCT v -> string (string_of_int v ^ "%")
      )

  (* properties *)

  method position : position =
    match sty >>> get "position" >>> as_string with
      | "fixed" -> `FIXED
      | "absolute" -> `ABSOLUTE
      | "relative" -> `RELATIVE
      | _ -> `RELATIVE
  method set_position (v : position) : unit =
    match v with
      | `FIXED -> sty >>> set "position" (string "fixed")
      | `RELATIVE -> sty >>> set "position" (string "relative")
      | `ABSOLUTE -> sty >>> set "position" (string "absolute")

  method z_index : int =
    sty >>> get "zIndex" >>> as_int
  method set_z_index (z : int) : unit =
    sty >>> set "zIndex" (int z)

  method background_color : color =
    sty >>> get "background" >>> as_string
  method set_background_color (c : color) : unit =
    sty >>> set "background" (string c)

end

let style obj =
  try
    Obj.obj (obj >>> get "caml_style" >>> as_block)
  with Failure "as_block" ->
    let style = new style obj in
      obj >>> set "caml_style" (inject (Block (Obj.repr style))) ;
      style
