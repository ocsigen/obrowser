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
//  JS                                                                       //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////


// Caml name: document
// Type:      unit -> t
RT.caml_js_node_document = function (v) {
    return box_abstract (document);
}
// Caml name: text
// Type:      string -> t
RT.caml_js_node_text = function (v) {
    var t = string_from_value (v);
    return box_abstract (document.createTextNode(t));;
}
// Caml name: element
// Type:      string -> t
RT.caml_js_node_element = function (i) {
    var id = string_from_value(i);
    return box_abstract (document.createElement(id));;
}
// Caml name: get_attribute
// Type:      t -> string -> string
RT.caml_js_node_get_attribute = function (n, a) {
    var node = unbox_abstract (n);
    var attr = string_from_value(a);
    var v = node[attr];
    return value_from_string (v == null ? "" : v);
}
// Caml name: remove_attribute
// Type:      t -> string -> unit
RT.caml_js_node_remove_attribute = function (n, a) {
    var node = unbox_abstract (n);
    var attr = string_from_value(a);
    node.removeAttribute(attr);
    return UNIT;
}
// Caml name: set_attribute
// Type:      t -> string -> string -> unit
RT.caml_js_node_set_attribute = function (n, a, v) {
    var node = unbox_abstract (n);
    var attr = string_from_value(a);
    var value = string_from_value(v);
    node.setAttribute (attr, value);
    return UNIT;
}
// Caml name: get_element_by_id
// Type:      t -> string -> t
RT.caml_js_node_get_element_by_id = function (n,i) {
    var node = unbox_abstract (n);
    var id = string_from_value(i);
    var obj = node.getElementById(id);
    if (obj == null)
	this.failwith ("No such node \"" + id + "\"");
    return box_abstract (obj);
}
// Caml name: register_event
// Type:      t -> string -> (unit -> unit) -> unit
RT.caml_js_node_register_event = function (n, e, closure) {
    var node = unbox_abstract (n);
    var event = string_from_value (e);
    function mk_cb (vm, cb) {
	return function () {
	    vm.thread_new (cb);
	    vm.run ();
	}
    }
    node[event] = mk_cb (this, closure);
    return UNIT;
}
// Caml name: clear_event
// Type:      t -> string -> unit
RT.caml_js_node_clear_event = function (n, e) {
    var node = unbox_abstract (n);
    var event = string_from_value (e);
    node[event] = null;
    return UNIT;
}

// Caml name: append
// Type:      t -> t -> unit
RT.caml_js_node_append = function (n, c) {
    var node = unbox_abstract (n);
    var content = unbox_abstract (c);
    try {
	node.appendChild (content);
    } catch (e) {
	this.failwith ("caml_js_node_append: " + e.message);
    }
    return UNIT;
}
// Caml name: remove
// Type:      t -> t -> unit
RT.caml_js_node_remove = function (n, c) {
    var node = unbox_abstract (n);
    var content = unbox_abstract (c);
    try {
	node.removeChild (content);
    } catch (e) {
	this.failwith ("caml_js_node_remove: " + e.message);
    }
    return UNIT;
}
// Caml name: children
// Type:      t -> t list
RT.caml_js_node_children = function (n) {
    var node = unbox_abstract (n);
    try {
	var res = nil;
	var cur = nil;
	var children = node.childNodes;
	for (c = 0;c < children.length;c++) {
	    if (res == nil) {
		res = cons (box_abstract (children[c]),nil);
		cur = res;
	    } else {
		cur.set(1, cons (box_abstract (children[c]),nil));
		cur = cur.get (1);
	    }
	}
	return res;
    } catch (e) {
	this.failwith ("caml_js_node_children: " + e.message);
    }
}
// Caml name: n_children
// Type:      t -> int
RT.caml_js_node_n_children = function (n) {
    var node = unbox_abstract (n);
    try {
	return node.childNodes.length;
    } catch (e) {
	this.failwith ("caml_js_node_n_children: " + e.message);
    }
}

// Caml name: child
// Type:      t -> int -> t
RT.caml_js_node_child = function (n, i) {
    var node = unbox_abstract (n);
    try {
	return box_abstract (node.childNodes[i]);
    } catch (e) {
	this.failwith ("caml_js_node_n_children: " + e.message);
    }
}


// Caml name: create
// Type:      unit -> t
RT.caml_js_fragment_create = function (v) {
    return box_abstract (document.createDocumentFragment ());
}

// Caml name: append
// Type:      t -> Node.t -> unit
RT.caml_js_fragment_append = function (f, n) {
    var fragment = unbox_abstract (f);
    var node = unbox_abstract (n);
    try {
	fragment.appendChild (node);
    } catch (e) {
	this.failwith ("caml_js_fragment_append: " + e.message);
    }
    return UNIT;
}

// Caml name: flush
// Type:      Node.t -> t -> unit
RT.caml_js_fragment_flush = function (n,f) {
    var fragment = unbox_abstract (f);
    var node = unbox_abstract (n);
    try {
	node.appendChild (fragment);
    } catch (e) {
	this.failwith ("caml_js_fragment_flush: " + e.message);
    }
    return UNIT;
}

// Caml name: http_get
// Type:      string -> string
RT.caml_js_http_get = function (url) {
    try {
	return value_from_string (http_get (string_from_value (url)));
    } catch (e) {
	this.failwith ("caml_js_http_get: " + e.message);
    }
}

// Caml name: http_post
// Type:      string -> string -> string
    RT.caml_js_http_post = function (url, post) {
    try {
	return value_from_string (http_post (string_from_value (url), string_from_value (post)));
    } catch (e) {
	this.failwith ("caml_js_http_post: " + e.message);
    }
}

// Caml name: alert
// Type:      string -> unit
RT.caml_js_alert = function (msg) {
    window.alert (string_from_value (msg));
    return UNIT;
}

// Caml name: params
// Type:      unit -> string array
RT.caml_js_params = function (v) {
    return this.argv;
}

// Caml name: exec
// Type:      string -> string array -> unit
RT.caml_js_exec = function (url, args) {
    var argv = [];
    for (var i = 0;i < args.size;i++)
	argv[i] = string_from_value (args.get(i));
    new VM(string_from_value (url), argv).run ();
    return UNIT;
}
