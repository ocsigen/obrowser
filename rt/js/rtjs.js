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
function caml_js_external (vsym, nargs) {
    var sym = string_from_value (vsym);
    try {
	this.prims[sym] = eval (sym);
    } catch (e) {
	return 0 /* None */;
    }
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
function caml_js_node_children (n) {
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
function caml_js_node_n_children (n) {
    var node = n;
    try {
	return node.childNodes.length;
    } catch (e) {
	this.failwith ("caml_js_node_n_children: " + e.message);
    }
}

// Caml name: child
// Type:      t -> int -> t
function caml_js_node_child (n, i) {
    var node = n;
    try {
	return node.childNodes[i];
    } catch (e) {
	this.failwith ("caml_js_node_n_children: " + e.message);
    }
}


// Caml name: create
// Type:      unit -> t
function caml_js_fragment_create (v) {
    return document.createDocumentFragment ();
}

// Caml name: append
// Type:      t -> Node.t -> unit
function caml_js_fragment_append (f, n) {
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
function caml_js_fragment_flush (n,f) {
    var fragment = f;
    var node = n;
    try {
	node.appendChild (fragment);
    } catch (e) {
	this.failwith ("caml_js_fragment_flush: " + e.message);
    }
    return UNIT;
}

// Caml name: alert
// Type:      string -> unit
function caml_js_alert (msg) {
    window.alert (string_from_value (msg));
    return UNIT;
}

// Caml name: params
// Type:      unit -> string array
function caml_js_params (v) {
    return this.argv;
}

// Caml name: exec
// Type:      string -> string array -> unit
function caml_js_exec (url, args) {
    var argv = [];
    for (var i = 0;i < args.size;i++)
	argv[i] = string_from_value (args.get(i));
    new VM(string_from_value (url), argv).run ();
    return UNIT;
}


// Caml name: http_get_with_status
// Type:      string -> (int *  string)
function caml_js_http_get_with_status (vurl) {
    var url = string_from_value (vurl);

    var xmlhttp = false;
    var vm = this;
    /* get request object */
    if (ie) {
        try {
	    xmlhttp = new ActiveXObject("Msxml2.XMLHTTP");
        } catch (e) {
	    try {
		xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
	    } catch (E) {
		throw new Error ("Unsupported Internet Explorer");
	    }
        }
    } else {
	xmlhttp = new XMLHttpRequest();
    }
    /* do request */
    try {
	xmlhttp.onreadystatechange = function () {
	    vm.thread_notify_all (xmlhttp);
	}
	xmlhttp.open("GET", url, true);
	xmlhttp.send(null);
	var cont = function  () {
	    if (xmlhttp.readyState != 4)
		vm.thread_wait (xmlhttp, cont);		
	    var b = mk_block (2, 0);
	    b.set (0, xmlhttp.status);
	    b.set (1, value_from_string (xmlhttp.responseText));
	    return b;
	}
	vm.thread_wait (xmlhttp, cont);
    } catch (e) {
	caml_catch(e);
	this.failwith("unable to load url " + url + ": " + e.message);
    }
}

// Caml name: http_post
// Type:      string -> string -> string -> (int *  string)
function caml_js_http_post (vurl, type, data) {
    var url = string_from_value (vurl);

    var xmlhttp = false;
    var vm = this;
    /* get request object */
    if (ie) {
        try {
	    xmlhttp = new ActiveXObject("Msxml2.XMLHTTP");
        } catch (e) {
	    try {
		xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
	    } catch (E) {
		throw new Error ("Unsupported Internet Explorer");
	    }
        }
    } else {
	xmlhttp = new XMLHttpRequest();
    }
    /* do request */
    try {
	xmlhttp.onreadystatechange = function () {
	    vm.thread_notify_all (xmlhttp);
	}
	xmlhttp.open("POST", url, true);
	xmlhttp.setRequestHeader("Content-Type", string_from_value (type));
	xmlhttp.send(string_from_value (data));
	var cont = function  () {
	    if (xmlhttp.readyState != 4)
		vm.thread_wait (xmlhttp, cont);		
	    var b = mk_block (2, 0);
	    b.set (0, xmlhttp.status);
	    b.set (1, value_from_string (xmlhttp.responseText));
	    return b;
	}
	vm.thread_wait (xmlhttp, cont);
    } catch (e) {
	caml_catch (e);
	this.failwith ("unable to load url " + url + ": " + e.message);
    }
}

// Caml name: dom_of_xml
// Type:      string -> JSOO.obj
function caml_js_dom_of_xml (str) 
{
    var sstr = string_from_value (str);
    try {
	try { //IE
	    xmlDoc = new ActiveXObject("Microsoft.XMLDOM");
	    xmlDoc.async = "false";
	    xmlDoc.loadXML(sstr);
	    return xmlDoc; 
	} catch(e) {
	    parser = new DOMParser();
	    xmlDoc = parser.parseFromString(sstr,"text/xml");
	    return xmlDoc;
	}
    } catch(e) { this.failwith ("unable to parse : " + e.message) }
}

// Caml name: pretty_xml_of_dom
// Type:      JSOO.obj -> string
function caml_js_pretty_xml_of_dom (o)
{
  try {
    var serializer = new XMLSerializer();
    var prettyString = XML(serializer.serializeToString(o)).toXMLString();
    return (value_from_string (prettyString)) ;
  } catch(e) { this.failwith ("unable to pretty print : " + e.message) }
}

// Caml name: xml_of_dom
// Type:      JSOO.obj -> string
function caml_js_xml_of_dom (o)
{
  try {
    var serializer = new XMLSerializer();
    var xml = serializer.serializeToString(o);
    return (value_from_string (xml)) ;
  } catch (e) { this.failwith ("unable to print : " + e.message) }
}

function basic_io_write (s) {
    var div = document.getElementById ("caml_io_console");
    if (div == null) {
	div = document.createElement ("DIV");
	div.style.position = "absolute";
	div.style.left = "5px";
	div.style.bottom = "5px";
	div.style.padding = "5px";
	div.style.backgroundColor = "lightgrey";
	div.style.color = "black";
	div.style.fontSize = "10px";
	div.style.whiteSpace = "pre";
	div.id = "caml_io_console";
	document.body.appendChild (div);
    }
    div.innerHTML += s;
    return UNIT;
}

// Caml name: basic_io_write
// Type:      string -> unit
function caml_basic_io_write (s) {
    basic_io_write (string_val (s));
    return UNIT;
}
