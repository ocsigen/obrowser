///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  Caml Virtual Machine in JavaScript                                       //
//  (C) 2007 Benjamin Canou (Benjamin.Canou@gmail.com)                       //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  This program is free software: you can redistribute it and/or modify     //
//  it under the terms of the GNU General Public License as published by     //
//  the Free Software Foundation, either version 3 of the License, or        //
//  (at your option) any later version.                                      //
//                                                                           //
//  This program is distributed in the hope that it will be useful,          //
//  but WITHOUT ANY WARRANTY; without even the implied warranty of           //
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            //
//  GNU General Public License for more details.                             //
//                                                                           //
//  You should have received a copy of the GNU General Public License        //
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.    //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  COMPILER FUNCTIONS (PROGRAM METADATA)                                    //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

// Caml name: global_data
// Type:      unit -> Obj.t array
RT["caml_get_global_data"] = function (unit) {
    return prg.data;
}
/*
// Caml name: realloc_global_data
// Type:      int -> unit
RT["caml_realloc_global"] = function () {
  throw new Error ("caml_realloc_global" + " not implemented");
}
// Caml name: static_alloc
// Type:      int -> string
RT["caml_static_alloc"] = function () {
  throw new Error ("caml_static_alloc" + " not implemented");
}
// Caml name: static_free
// Type:      string -> unit
RT["caml_static_free"] = function () {
  throw new Error ("caml_static_free" + " not implemented");
}
// Caml name: static_resize
// Type:      string -> int -> string
RT["caml_static_resize"] = function () {
  throw new Error ("caml_static_resize" + " not implemented");
}
// Caml name: static_release_bytecode
// Type:      string -> int -> unit
RT["caml_static_release_bytecode"] = function () {
  throw new Error ("caml_static_release_bytecode" + " not implemented");
}
// Caml name: reify_bytecode
// Type:      string -> int -> closure
RT["caml_reify_bytecode"] = function () {
  throw new Error ("caml_reify_bytecode" + " not implemented");
}
// Caml name: invoke_traced_function
// Type:      Obj.t -> Obj.t -> Obj.t -> Obj.t
RT["caml_invoke_traced_function"] = function () {
  throw new Error ("caml_invoke_traced_function" + " not implemented");
}
*/
// Caml name: get_section_table
// Type:      unit -> (string * Obj.t) list
RT["caml_get_section_table"] = function () {
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
