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
//  PLATFORM SPECIFIC EXTENSIONS                                             //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

register_dll ("cvmjs");

var link_id = 0;
var link_callbacks = new Array ();

// Caml name: link_to_callback
// Type:      (unit-> unit) -> string -> unit
RT["cvmjs_link_to_callback"] = function (clos, txt) {
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
RT["cvmjs_alert"] = function (s) {
    alert (string_from_value (s));
    return UNIT;
}

// Caml name: repr
// Type:      'a -> string
RT["cvmjs_repr"] = function (v) {
    return value_from_string (repr (v, 100));
}

// Caml name: repr_exc
// Type:      exn -> string
RT["cvmjs_repr_exc"] = function (v) {
    return value_from_string (string_from_value (v.get (0)) +
			      " " + repr (v.get (1), 100));
}

// Caml name: clear_console
// Type:      unit -> unit
RT["cvmjs_clear_console"] = function () {
    channels[1].con.clear ();
    return UNIT;
}
