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
//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//  JAVASCRIPT EVENTS WRAPPER                                               //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

var custom_handlers = new Array ();
var custom_handlers_always = new Array ();
var document_managed_handlers = new Array (
    "onmousedown", "onmousemove", "onmouseup",
    "onresize", "onmouseover", "onmouseout",
    "onkeyup", "onkeydown"
);
var window_managed_handlers = new Array (
    "onresize"
);

function custom_handler (name, evt, opt) {
    if (custom_handlers[name][evt.target.id] != null) {
	custom_handlers[name][evt.target.id](evt, opt);
	try { evt.preventDefault (); } catch (e) {}
    }
    for (i in custom_handlers_always[name]) {
	if (custom_handlers_always[name][i] != null) {
	    custom_handlers_always[name][i](evt, opt);
	    try { evt.preventDefault (); } catch (e) {}
	}
    }
}

function make_custom_handler (name) {
    return function (evt, opt) {
	custom_handler (name, evt, opt);
    }
}

for (i in document_managed_handlers) {
    function h (name) {
	return function (evt, opt) {
	    custom_handler (name, evt, opt);
	}
    }
    document[document_managed_handlers[i]] = h (document_managed_handlers[i]);
    custom_handlers[document_managed_handlers[i]] = new Array ();
    custom_handlers_always[document_managed_handlers[i]] = new Array ();
}

for (i in window_managed_handlers) {
    function h (name) {
	return function (evt) {
	    custom_handler (name, evt);
	}
    }
    window[window_managed_handlers[i]] = h (window_managed_handlers[i]);
    custom_handlers[window_managed_handlers[i]] = new Array ();
    custom_handlers_always[window_managed_handlers[i]] = new Array ();
}

function link_handler (name, id, h) {
    if (custom_handlers[name] == null) {
	custom_handlers[name] = new Array ();
	custom_handlers_always[name] = new Array ();
    }
    custom_handlers[name][id] = h;
    document.getElementById (id)[name] = make_custom_handler (name);
}

function unlink_handler (name, id) {
    custom_handlers[name][id] = null;
}

function link_handler_always (name, id, h) {
    if (custom_handlers_always[name] == null) {
	custom_handlers[name] = new Array ();
	custom_handlers_always[name] = new Array ();
    }
    custom_handlers_always[name][id] = h;
    document.getElementById (id)[name] = make_custom_handler (name);
}

function unlink_handler_always (name, id, h) {
    custom_handlers_always[name][id] = null;
}

function emit (name, id, evt, opt) {
    if (custom_handlers[name] != null) {
	if (custom_handlers[name][id] != null) {
	    custom_handlers[name][id](evt, opt);
	}
    }
    if (custom_handlers_always[name] != null) {
	for (i in custom_handlers_always[name]) {
	    if (custom_handlers_always[name][i] != null) {
		custom_handlers_always[name][i](evt, opt);
	    }
	}
    }
}

function connect (name, id, obj, slot, opt, alw) {
    function cb (obj, slot, opt) {
	return function (evt) {
	    obj[slot].call(obj, evt, opt);
	}
    }
    if (alw)
	link_handler_always (name, id, cb (obj, slot, opt));
    else
	link_handler (name, id, cb (obj, slot, opt));
}

function disconnect (name, id, obj, slot, opt) {
    unlink_handler (name, id);
    unlink_handler_always (name, id);
}

function connect_cb (name, id, f) {
    link_handler (name, id, f);
}
