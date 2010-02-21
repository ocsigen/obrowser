/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

// tags

#define NO_SCAN_TAG        251
#define FORWARD_TAG        250
#define INFIX_TAG          249
#define OBJECT_TAG         248
#define CLOSURE_TAG        247
#define LAZY_TAG           246
#define ABSTRACT_TAG       251
#define STRING_TAG         252
#define DOUBLE_TAG         253
#define DOUBLE_ARRAY_TAG   254
#define CUSTOM_TAG         255

// blocks

function Block(size, tag) {
    this.size = size;
    this.tag = tag;
    this.content = [];
    this.offset = 0;
}

Block.prototype.get = function (i) {
    return this.content[this.offset + i];
}

Block.prototype.set = function (i, v) {
    this.content[this.offset + i] = v;
}

/* enables the simulation of C pointer arithmetics */
Block.prototype.shift = function (o) {
    var nsize = this.size - o >= 0 ? this.size - o : 0;
    var b = new Block (nsize, this.tag);
    b.content = this.content;
    b.offset = this.offset + o;
    return b;
}

#define is_block(b)         (b instanceof Block)
#define mk_block(size, tag) (new Block (size, tag))
#define field(b,n)          (b).get(n)
#define store_field(b,n,v)  (b).set(n,v)

var ATOM = mk_block (0, 0);

function pair (v0, v1) {
    var b = new Block (2, 0);
    b.set (0, v0);
    b.set (1, v1);
    return b;
}

function singleton (v0) {
    var b = new Block (1, 0);
    b.set (0, v0);
    return b;
}

function box_abstract (v0) {
    var b = new Block (1, ABSTRACT_TAG);
    b.set (0, v0);
    return b;
}

#define unbox_abstract(v) v.get (0)

#define unbox_code(v) (v.tag == CLOSURE_TAG ? v.get (0):v)

// immediates

#define is_long(b) (!(b instanceof Block))

#define UNIT   0

#define FALSE  0
#define TRUE   1

#define mk_bool(v) (v?TRUE:FALSE)

// lists

var nil = 0;
var cons = pair;

// floats

function float_of_int (x) {
    var b = new Block (1, DOUBLE_TAG);
    b.set (0, Number (x));
    return b;
}

function box_float (x) {
    var b = new Block (1, DOUBLE_TAG);
    b.set (0, Number (x));
    return b;
}

#define unbox_float(x) x.get (0)

#define int_of_float(x) Math.round (x.get (0))

function float_of_bytes (bytes) {
    /* sign & exponent */
    var sign = ((bytes[0] >> 7) == 1);
    var exponent = (((bytes[0] & 0x7F) << 4) | (bytes[1] >> 4 )) - 1023;
    /* mantissa in a bool array */
    var ba = [];
    for (var b = 1;b < 8;b++)
	for (var d = 0;d < 8;d++)
	    ba[(b - 1) * 8 + d - 4] = (((bytes[b] >> (7 - d)) & 1) == 1);
    /* proceed */
    var m = Number (1);
    for (var i = 0;i < 52;i++)
	if (ba[i])
	    m += Math.pow (2, -(i + 1));
    return box_float ((sign ? (-1) : 1) * m * Math.pow (2, exponent));
}

function bytes_of_float (x) {
    var x = unbox_float (x);
    var e = Math.ceil (Math.log (Math.abs (x)) / Math.log (2));
    var m = Math.abs (x * Math.pow (2, -e)) * 2 - 1;
    e += 1022;
    var bits = [];
    bits[0] = (x > 0);
    for (var i = 0;i <= 52 ; i++) {
	bits [11 + i] = (m >= 1);
	m = (m - Math.floor (m)) * 2;
    }
    for (var i = 0;i <= 10 ; i++) {
	bits [11 - i] = (((e >>> i) & 1) == 1);
    }
    var bytes = [0,0,0,0,0,0,0,0];
    for (var i = 0;i < 8 ; i++) {
	for (var j = 0;j < 8 ; j++) {
	    bytes[i] = (bytes[i] * 2) | (bits[8 * i + j] ? 1 : 0);
	}
    }
    return bytes;
}

// strings

#include <utf8.js>

var utf8_enabled = TRUE;

caml_js_enable_utf8 /* : bool -> unit */ = function (v) {
    utf8_enabled = v;
    return UNIT;
}

caml_js_utf8_enabled /* : unit -> bool */ = function () {
    return utf8_enabled;
}

function value_from_string (s) {
    if (utf8_enabled == FALSE) {
	var b = mk_block (s.length + 1, STRING_TAG);
	for (var i = 0;i < s.length;i++) {
	    b.set(i,s.charCodeAt (i));
	}
	b.set(i, 0);
	return b;
    } else {
	return encode_utf8 (s);
    }
}
function string_from_value (v) {
    if (utf8_enabled == FALSE) {
	var s = "";
	for (var i = 0;i < v.size - 1;i++) {
	    s += String.fromCharCode (v.get (i));
	}
	return s;
    } else {
	return decode_utf8 (v);
    }
}

function string_array (a) {
    var b = new Block (a.length);
    for (var i = 0;i < a.length;i++)
	b.set (i, value_from_string (a[i]));
    return b;
}

// utils

/* block from an array of values */
function mk_array_from_js (s) {
    var b = mk_block (s.length, 0);
    for (var i = 0;i < s.length;i++) {
	b.set(i,s[i]);
    }
    return b;
}

/* (js) string representation of a value
   (limit of blocks = limit, does not handle cycles) */

function repr (v, limit) {
    var s = "";
    function string_repr_rec (v) {
	if (is_long (v)) {
	    s += sprintf ("0x%X", v);
	} else {
	    switch (v.tag) {
	    case STRING_TAG:
		s += "\"" + string_from_value (v) + "\"";
		break;
	    case DOUBLE_TAG:
		s += v.get (0).toExponential ();
		break;
	    default: {
		s += sprintf ("[(0x%02X) ", v.tag);
		for (var i = 0;i < v.size - 1 && i < limit;i++) {
		    string_repr_rec (v.get (i));
		    s += ", ";
		}
		if (i >= limit) {
		    s += "...";
		} else {
		    string_repr_rec (v.get (i));
		}
		s += "]";
	    }
	    }
	}
    }
    string_repr_rec (v);
    return s;
}

