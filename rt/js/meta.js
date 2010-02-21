/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

// Caml name: global_data
// Type:      unit -> Obj.t array
function caml_get_global_data (unit) {
    return prg.data;
}
/*
// Caml name: realloc_global_data
// Type:      int -> unit
function caml_realloc_global () {
  throw new Error ("caml_realloc_global" + " not implemented");
}
// Caml name: static_alloc
// Type:      int -> string
function caml_static_alloc () {
  throw new Error ("caml_static_alloc" + " not implemented");
}
// Caml name: static_free
// Type:      string -> unit
function caml_static_free () {
  throw new Error ("caml_static_free" + " not implemented");
}
// Caml name: static_resize
// Type:      string -> int -> string
function caml_static_resize () {
  throw new Error ("caml_static_resize" + " not implemented");
}
// Caml name: static_release_bytecode
// Type:      string -> int -> unit
function caml_static_release_bytecode () {
  throw new Error ("caml_static_release_bytecode" + " not implemented");
}
// Caml name: reify_bytecode
// Type:      string -> int -> closure
function caml_reify_bytecode () {
  throw new Error ("caml_reify_bytecode" + " not implemented");
}
// Caml name: invoke_traced_function
// Type:      Obj.t -> Obj.t -> Obj.t -> Obj.t
function caml_invoke_traced_function () {
  throw new Error ("caml_invoke_traced_function" + " not implemented");
}
*/
// Caml name: get_section_table
// Type:      unit -> (string * Obj.t) list
function caml_get_section_table () {
    var res = 0;
    for (var i = 0;i < prg.nsections;i++) {
	var name = value_from_string (prg.sections[i].name);
	// Über dégueulasse
	var tsection = prg.read_section (prg.sections[i].name);
	var section;
	try {
	    // set to deserialised value
	    section = input_val (tsection, function(e){throw new Error(e)});
	} catch (e) {
	    // otherwise, expected is a string... >_<
	    section = mk_block (prg.sections[i].len + 1, 0);
	    for (var c = 0;c < prg.sections[i].len;c++)
		section.set (c, tsection[c]);
	    section.set (prg.sections[i].len, 0);
	}
	res = pair (pair (name, section), res);
    }
    return res;
}
