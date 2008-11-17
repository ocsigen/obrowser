/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

// Caml name: get_argv
// Type:      unit -> string * string array
RT.caml_sys_get_argv = function (unit) {
    return pair (this.program_name, this.argv);
}

var init_time = (new Date ()).getTime () * 0.001;

// Caml name: time
// Type:      unit -> float
RT.caml_sys_time = function (unit) {
    return box_float ((new Date ()).getTime () * 0.001 - init_time);
}

// Caml name: get_config
// Type:      unit -> string * int
RT.caml_sys_get_config = function (unit) {
    var b = mk_block (2, 0);
    b.set (0, value_from_string ("Unix"));
    b.set (1, 32);
    return b;
}

// Caml name: getenv
// Type:      string -> string
RT.caml_sys_getenv = function (v) {0
    caml_raise_constant (NOT_FOUND_EXN);
}

// Caml name: random_seed
// Type:      unit -> int
RT.caml_sys_random_seed = function (unit) {
    return Math.floor(Math.random() * Math.pow(2, 31));
}


// Caml name: file_exists
// Type:      string -> bool
RT.caml_sys_file_exists = function (name) {
    try {
	http_get ("exists/" + string_from_value (name),
		  function (e) {throw e;});
	return TRUE;
    } catch (e) {
	return FALSE;
    }
}
/*
// Caml name: is_directory
// Type:      string -> bool
RT.caml_sys_is_directory = function () {
  throw new Error ("caml_sys_is_directory" + " not implemented");
}
// Caml name: remove
// Type:      string -> unit
RT.caml_sys_remove = function () {
  throw new Error ("caml_sys_remove" + " not implemented");
}
// Caml name: rename
// Type:      string -> string -> unit
RT.caml_sys_rename = function () {
  throw new Error ("caml_sys_rename" + " not implemented");
}
// Caml name: command
// Type:      string -> int
RT.caml_sys_system_command = function () {
  throw new Error ("caml_sys_system_command" + " not implemented");
}
// Caml name: chdir
// Type:      string -> unit
RT.caml_sys_chdir = function () {
  throw new Error ("caml_sys_chdir" + " not implemented");
}
// Caml name: getcwd
// Type:      unit -> string
RT.caml_sys_getcwd = function () {
  throw new Error ("caml_sys_getcwd" + " not implemented");
}
// Caml name: readdir
// Type:      string -> string array
RT.caml_sys_read_directory = function () {
  throw new Error ("caml_sys_read_directory" + " not implemented");
}
// Caml name: signal
// Type:      int -> signal_behavior -> signal_behavior
RT.caml_install_signal_handler = function () {
  throw new Error ("caml_install_signal_handler" + " not implemented");
}
*/
