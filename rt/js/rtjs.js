/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

// Caml name: js_external
// Type:      string -> int -> ('a -> 'b) option
RT.caml_js_external = function (vsym, nargs) {
    var sym = string_from_value (vsym);
    if (RT[sym] == null)
	return UNIT;
    this.prims[sym] = RT[sym];
    var cl;
    switch (nargs) {
    case 1:
	cl = mk_block (1, CLOSURE_TAG);
	cl.set (0, mk_block (5, 0));
	cl.get (0).set (0, IACC0);
	cl.get (0).set (1, IJS_CALL1);
	cl.get (0).set (2, sym);
	cl.get (0).set (3, IRETURN);
	cl.get (0).set (4, 1);
	break;
    case 2:
	var cl = mk_block (1, CLOSURE_TAG);
	cl.set (0, mk_block (8, 0));
	cl.get (0).set (0, IGRAB);
	cl.get (0).set (1, IACC1);
	cl.get (0).set (2, IACC1);
	cl.get (0).set (3, IPUSHACC1);
	cl.get (0).set (4, IJS_CALL2);
	cl.get (0).set (5, sym);
	cl.get (0).set (6, IRETURN);
	cl.get (0).set (7, 2);
	break;
    case 3:
	var cl = mk_block (1, CLOSURE_TAG);
	cl.set (0, mk_block (9, 0));
	cl.get (0).set (0, IGRAB);
	cl.get (0).set (1, IACC2);
	cl.get (0).set (2, IACC2);
	cl.get (0).set (3, IPUSHACC2);
	cl.get (0).set (4, IPUSHACC2);
	cl.get (0).set (5, IJS_CALL3);
	cl.get (0).set (6, sym);
	cl.get (0).set (7, IRETURN);
	cl.get (0).set (8, 3);
	break;
    default:
	this.failwith ("unhandled number of arguments for dyn external");
    }

    var some_cl = mk_block (1, 0);
    some_cl.set (0, cl)
    return some_cl;
}

// Caml name: children
// Type:      t -> t list
RT.caml_js_node_children = function (n) {
    var node = n;
    try {
	var res = nil;
	var cur = nil;
	var children = node.childNodes;
	for (c = 0;c < children.length;c++) {
	    if (res == nil) {
		res = cons (children[c],nil);
		cur = res;
	    } else {
		cur.set(1, cons (children[c],nil));
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
    var node = n;
    try {
	return node.childNodes.length;
    } catch (e) {
	this.failwith ("caml_js_node_n_children: " + e.message);
    }
}

// Caml name: child
// Type:      t -> int -> t
RT.caml_js_node_child = function (n, i) {
    var node = n;
    try {
	return node.childNodes[i];
    } catch (e) {
	this.failwith ("caml_js_node_n_children: " + e.message);
    }
}


// Caml name: create
// Type:      unit -> t
RT.caml_js_fragment_create = function (v) {
    return document.createDocumentFragment ();
}

// Caml name: append
// Type:      t -> Node.t -> unit
RT.caml_js_fragment_append = function (f, n) {
    var fragment = f;
    var node = n;
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
    var fragment = f;
    var node = n;
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
