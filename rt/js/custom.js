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

function mk_int64 (t) { return mk_custom (int64_ops, t); }

function int64_compare (a, b) {
    var t = int64_sub (a, b).get (1);
    var is_zero = true;
    for (var i = 0;i < 8;i++)
	if (t[i] != 0) is_zero = false;
    if (is_zero)
	return 0;
    return (((t[0] >> 7) == 1) ? -1 : 1);
}
function int64_add (a, b) {
    a = a.get (1);
    b = b.get (1);
    r = [];
    var carry = 0;
    for (var i = 0;i < 8;i++) {
	r[i] = a[i] + b[i] + carry;
	carry = r[i] >> 8;
	r[i] &= 0xFF;
    }
    return mk_int64 (r);
}
function int64_neg (a) {
    a = a.get (1);
    var r = new Int64 ();
    r = [];
    for (var i = 0;i < 8;i++)
	r[i] = a[i] ^ 0xFF;
    var carry = 1;
    for (var i = 0;i < 8;i++) {
	r[i] = a[i] +carry;
	carry = r[i] >> 8;
	r[i] &= 0xFF;
    }
    return mk_custom (int64_ops, r);
}
function int64_sub (a, b) {
    return int64_add (a, int64_neg (b));
}
caml_int64_of_int = int64_of_int = function (x) {
    var t = [];
    for (var i = 0;i < 4;i++)
	t[3 - i] = (x < 0) ? 0xFF : 0x00;
    for (var i = 0;i < 4;i++)
	t[7 - i] = (x >> (i * 8)) & 0xFF;
    return mk_int64 (t);
}
function int64_of_bytes (bytes) {
    return mk_int64 (r);
}
function int64_to_bytes (v) {
    return v.get (1);
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
	return mk_int64 (t);
    }
};

register_custom (int64_ops);
