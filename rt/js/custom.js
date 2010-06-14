/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

var customs = new Array ();

function register_custom (ops) {
    customs[ops.id] = ops;
}

function find_custom (id) {
    return customs[id];
}

function mk_custom (ops, val) {
    var b = new Block (2, CUSTOM_TAG);
    b.set (0, ops);
    b.set (1, val);
    return b;
}

/* int32 */

int32_ops = {
    id : "_i",
    compare :  function (a,b) {
	return (a - b);
    },
    hash : function (a) {
	return a;
    },
    serialize : function (writer) {
	throw new Error ("int32 not supported yet");
    },
    deserialize : function (reader) {
	return mk_custom (int32_ops, reader.read32u ());
    }
};

register_custom (int32_ops);

// Caml name: Int32.neg
// Caml type: int32 -> int32
function caml_int32_neg (a) {
    var a = a.get (1);
    return mk_custom (int32_ops, -a);
}
// Caml name: Int32.add
// Caml type: int32 -> int32 -> int32
function caml_int32_add (va,vb) {
    var a = va.get (1);
    var b = vb.get (1) ;
    return mk_custom (int32_ops, a + b);
}
// Caml name: Int32.sub
// Caml type: int32 -> int32 -> int32
function caml_int32_sub (a,b) {
    var a = a.get (1);
    var b = b.get (1) ;
    return mk_custom (int32_ops, a - b);
}
// Caml name: Int32.mul
// Caml type: int32 -> int32 -> int32
function caml_int32_mul (a,b) {
    var a = a.get (1);
    var b = b.get (1) ;
    return mk_custom (int32_ops, a * b);
}
// Caml name: Int32.div
// Caml type: int32 -> int32 -> int32
function caml_int32_div (a,b) {
    var a = a.get (1);
    var b = b.get (1) ;
    return mk_custom (int32_ops, a / b);
}
// Caml name: Int32.rem
// Caml type: int32 -> int32 -> int32
function caml_int32_mod (a,b) {
    var a = a.get (1);
    var b = b.get (1) ;
    return mk_custom (int32_ops, a % b);
}
// Caml name: Int32.logand
// Caml type: int32 -> int32 -> int32
function caml_int32_and (a,b) {
    var a = a.get (1);
    var b = b.get (1) ;
    return mk_custom (int32_ops, a & b);
}
// Caml name: Int32.logor
// Caml type: int32 -> int32 -> int32
function caml_int32_or (a,b) {
    var a = a.get (1);
    var b = b.get (1) ;
    return mk_custom (int32_ops, a | b);
}
// Caml name: Int32.logxor
// Caml type: int32 -> int32 -> int32
function caml_int32_xor (a,b) {
    var a = a.get (1);
    var b = b.get (1) ;
    return mk_custom (int32_ops, a ^ b);
}
// Caml name: Int32.shift_left
// Caml type: int32 -> int -> int32
function caml_int32_lsl (a,b) {
    var a = a.get (1);
    return mk_custom (int32_ops, a << b);
}
// Caml name: Int32.shift_right
// Caml type: int32 -> int -> int32
function caml_int32_asr (a,b) {
    var a = a.get (1);
    return mk_custom (int32_ops, a >> b);
}
// Caml name: Int32.shift_right_logical
// Caml type: int32 -> int -> int32
function caml_int32_lsr (a,b) {
    var a = a.get (1);
    return mk_custom (int32_ops, a >>> b);
}
// Caml name: Int32.of_int
// Caml type: int -> int32
function caml_int32_of_int (a) {
    return mk_custom (int32_ops, a);
}
// Caml name: Int32.to_int
// Caml type: int32 -> int
function caml_int32_to_int (a) {
    return a.get (1);
}
// Caml name: Int32.format
// Caml type: string -> int32 -> string
function caml_int32_format (f, v) {
    return caml_format_int.call (this, f, v.get (1));
}
// Caml name: Int32.of_string
// Caml type: string -> int32
function caml_int32_of_string (s) {
    return mk_custom (int32_ops, caml_int_of_string.call (this, s));
}


/* nativeint */

nativeint_ops = {
    id : "_n",
    compare : function (a,b) {
	return (a - b);
    },
    hash : function (a) {
	return a;
    },
    serialize : function (writer) {
	throw new Error ("nativeint not supported yet");
    },
    deserialize : function (reader) {
	var l = reader.read8u ();
	if (l == 1)
	    return mk_custom (nativeint_ops, reader.read32u ());
	else
	    throw new Error ("> 32 bits native int not supported yet");
    }
};

register_custom (nativeint_ops);

function caml_nativeint_shift_left (x, s) {
    return (x << s);
}

function caml_nativeint_sub (a,b) {
    return (a - b);
}

/* int64 */

#include "Int64.js"

function val_int64 (t) { return mk_custom (int64_ops, t); }

caml_int64_compare = int64_compare = function (a, b) {
    return a.get (1).compareTo (b.get (1));
}
caml_int64_add = int64_add = function (a, b) {
    return val_int64 (a.get (1).add (b.get (1)));
}
caml_int64_neg = int64_neg = function (a) {
    return val_int64 (a.get (1).neg ());
}
caml_int64_sub = int64_sub = function (a, b) {
    return val_int64 (a.get (1).sub (b.get (1)));
}
caml_int64_mul = int64_mul = function (a, b) {
    return val_int64 (a.get (1).mul (b.get (1)));
}
caml_int64_div = int64_div = function (a, b) {
    return val_int64 (a.get (1).div (b.get (1)));
}
caml_int64_mod = int64_mod = function (a, b) {
    return val_int64 (a.get (1).mod (b.get (1)));
}
caml_int64_and = int64_and = function (a, b) {
    return val_int64 (a.get (1).and (b.get (1)));
}
caml_int64_or = int64_or = function (a, b) {
    return val_int64 (a.get (1).or (b.get (1)));
}
caml_int64_xor = int64_xor = function (a, b) {
    return val_int64 (a.get (1).xor (b.get (1)));
}
caml_int64_lsl = int64_lsl = function (a, b) {
    return val_int64 (a.get (1).lsl (b.get (1)));
}
caml_int64_lsr = int64_lsr = function (a, b) {
    return val_int64 (a.get (1).lsr (b.get (1)));
}
caml_int64_asr = int64_asr = function (a, b) {
    return val_int64 (a.get (1).asr (b.get (1)));
}
caml_int64_of_int32 = int64_of_int32 = function (x) {
    return val_int64 (new Int64 (x.get (1)));
}
caml_int64_to_int32 = int64_to_int32 = function (x) {
    return val_custom (int32_ops, x.get (1).lo);
}
caml_int64_of_int = int64_of_int = function (x) {
    return val_int64 (new Int64 (x));
}
caml_int64_to_int = int64_to_int = function (x) {
    return x.get (1).lo;
}
caml_int64_of_float = int64_of_float = function (x) {
    return val_int64 (new Int64 (Math.floor (x)));
}
caml_int64_to_float = int64_to_float = function (x) {
    return val_float (x.get (1).lo + x.get (1).hi * (1 << 30) * 4);
}
function caml_int64_format (fmt, x) {
    var fmt = string_from_value (fmt);
    var t = fmt[fmt.length - 1];
    var n = int64_val (x).toString (t == 'd' ? 10 : (t == 'o' ? 8 : (t == 'x' ? 16 : 10 /* err */)));
    var l = 0, c = '0';
    if (fmt.length == 3) {
	l = fmt.charCodeAt (1) - "0".charCodeAt (0);
    } else {
	if (fmt.length >= 4) {
	    c = fmt.charAt(1);
	    for (var i = 2;i <= fmt.length - 2;i++)
		l = l * 10 + (fmt.charCodeAt (i) - "0".charCodeAt (0));
	}
    }
    var rem = l - n.length;
    for (var i = 0;i < rem;i++)
	n = c + n;
    return value_from_string (n);
}
function caml_int64_of_string (s) {
    return val_int64 (parseInt64 (s));
}

function int64_of_bytes (bytes) {
    var lo = bytes[7] | (bytes[6] << 8) | (bytes[5] << 16) | (bytes[4] << 16);
    var hi = bytes[3] | (bytes[2] << 8) | (bytes[1] << 16) | (bytes[0] << 16);
    return val_int64 (new Int64 (lo, hi));
}
function int64_to_bytes (v) {
    return [
	(v.get (1).hi >> 24) & 0xFF,
	(v.get (1).hi >> 16) & 0xFF,
	(v.get (1).hi >> 8) & 0xFF,
	(v.get (1).hi >> 0) & 0xFF,
	(v.get (1).lo >> 24) & 0xFF,
	(v.get (1).lo >> 16) & 0xFF,
	(v.get (1).lo >> 8) & 0xFF,
	(v.get (1).lo >> 0) & 0xFF
    ];
}

int64_ops = {
    id : "_j",
    compare : function (a,b) {
	int64_compare (a, b);
    },
    hash : function (a) {
	var t = a.get (1);
	return t[7] | (t[6] << 8) | (t[5] << 16) | (t[4] << 24);
    },
    serialize : function (v, writer) {
	var t = int64_to_bytes (v);
	for (var j = 0;j < 8;j++)
	    writer.write (8, t[j]);
	writer.size_32 += 2 + ((8 + 3) >> 2);
	writer.size_64 += 2 + ((8 + 7) >> 3);
    },
    deserialize : function (reader) {
	var t = [];
	for (var j = 0;j < 8;j++)
	    t[j] = reader.read8u();
	return int64_of_bytes (t);
    }
};

register_custom (int64_ops);
