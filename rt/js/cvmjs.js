/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

register_dll ("cvmjs");

var link_id = 0;
var link_callbacks = new Array ();

// Caml name: link_to_callback
// Type:      (unit-> unit) -> string -> unit
function cvmjs_link_to_callback (clos, txt) {
    link_callbacks[link_id] = function () {
	prg.thread_new (clos);
	if (prg.status == VM_WAITING)
	    vm_run ();
    }
    channels[1].con
    .puts ("<a class='cbk' " +
	   "     href='javascript:link_callbacks[" + link_id + "] ()'>" +
	   string_from_value (txt) + "</a>");
    link_id++;
    return UNIT;
}

// Caml name: alert
// Type:      string -> unit
function cvmjs_alert (s) {
    alert (string_from_value (s));
    return UNIT;
}

// Caml name: repr
// Type:      'a -> string
function cvmjs_repr (v) {
    return value_from_string (repr (v, 100));
}

// Caml name: repr_exc
// Type:      exn -> string
function cvmjs_repr_exc (v) {
    return value_from_string (string_from_value (v.get (0)) +
			      " " + repr (v.get (1), 100));
}

// Caml name: clear_console
// Type:      unit -> unit
function cvmjs_clear_console () {
    channels[1].con.clear ();
    return UNIT;
}
