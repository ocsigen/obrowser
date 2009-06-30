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
RT.caml_int32_neg = function (a) {
    var a = a.get (1);
    return mk_custom (int32_ops, -a);
}
// Caml name: Int32.add
// Caml type: int32 -> int32 -> int32
RT.caml_int32_add = function (va,vb) {
    var a = va.get (1);
    var b = vb.get (1) ;
    return mk_custom (int32_ops, a + b);
}
// Caml name: Int32.sub
// Caml type: int32 -> int32 -> int32
RT.caml_int32_sub = function (a,b) {
    var a = a.get (1);
    var b = b.get (1) ;
    return mk_custom (int32_ops, a - b);
}
// Caml name: Int32.mul
// Caml type: int32 -> int32 -> int32
RT.caml_int32_mul = function (a,b) {
    var a = a.get (1);
    var b = b.get (1) ;
    return mk_custom (int32_ops, a * b);
}
// Caml name: Int32.div
// Caml type: int32 -> int32 -> int32
RT.caml_int32_div = function (a,b) {
    var a = a.get (1);
    var b = b.get (1) ;
    return mk_custom (int32_ops, a / b);
}
// Caml name: Int32.rem
// Caml type: int32 -> int32 -> int32
RT.caml_int32_mod = function (a,b) {
    var a = a.get (1);
    var b = b.get (1) ;
    return mk_custom (int32_ops, a % b);
}
// Caml name: Int32.logand
// Caml type: int32 -> int32 -> int32
RT.caml_int32_and = function (a,b) {
    var a = a.get (1);
    var b = b.get (1) ;
    return mk_custom (int32_ops, a & b);
}
// Caml name: Int32.logor
// Caml type: int32 -> int32 -> int32
RT.caml_int32_or = function (a,b) {
    var a = a.get (1);
    var b = b.get (1) ;
    return mk_custom (int32_ops, a | b);
}
// Caml name: Int32.logxor
// Caml type: int32 -> int32 -> int32
RT.caml_int32_xor = function (a,b) {
    var a = a.get (1);
    var b = b.get (1) ;
    return mk_custom (int32_ops, a ^ b);
}
// Caml name: Int32.shift_left
// Caml type: int32 -> int -> int32
RT.caml_int32_lsl = function (a,b) {
    var a = a.get (1);
    return mk_custom (int32_ops, a << b);
}
// Caml name: Int32.shift_right
// Caml type: int32 -> int -> int32
RT.caml_int32_asr = function (a,b) {
    var a = a.get (1);
    return mk_custom (int32_ops, a >> b);
}
// Caml name: Int32.shift_right_logical
// Caml type: int32 -> int -> int32
RT.caml_int32_lsr = function (a,b) {
    var a = a.get (1);
    return mk_custom (int32_ops, a >>> b);
}
// Caml name: Int32.of_int
// Caml type: int -> int32
RT.caml_int32_of_int = function (a) {
    return mk_custom (int32_ops, a);
}
// Caml name: Int32.to_int
// Caml type: int32 -> int
RT.caml_int32_to_int = function (a) {
    return a.get (1);
}
// Caml name: Int32.format
// Caml type: string -> int32 -> string
RT.caml_int32_format = function (f, v) {
    return RT.caml_format_int.call (this, f, v.get (1));
}
// Caml name: Int32.of_string
// Caml type: string -> int32
RT.caml_int32_of_string = function (s) {
    return mk_custom (RT.caml_int_of_string.call (this, s));
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

RT.caml_nativeint_shift_left = function (x, s) {
    return (x << s);
}

RT.caml_nativeint_sub = function (a,b) {
    return (a - b);
}

/* int64 */

function Int64 () {}

int64_compare = function (a, b) {
    var t = int64_sub (a, b).get (1).t;
    var is_zero = true;
    for (var i = 0;i < 8;i++)
	if (t[i] != 0) is_zero = false;
    if (is_zero)
	return 0;
    return (((t[0] >> 7) == 1) ? -1 : 1);
}
int64_add = function (a, b) {
    a = a.get (1);
    b = b.get (1);
    var r = new Int64 ();
    r.t = [];
    var carry = 0;
    for (var i = 0;i < 8;i++) {
	r.t[i] = a.t[i] + b.t[i] + carry;
	carry = r.t[i] >> 8;
	r.t[i] &= 0xFF;
    }
    return mk_custom (int64_ops, r);
}
int64_neg = function (a) {
    a = a.get (1);
    var r = new Int64 ();
    r.t = [];
    for (var i = 0;i < 8;i++)
	r.t[i] = a.t[i] ^ 0xFF;
    var carry = 1;
    for (var i = 0;i < 8;i++) {
	r.t[i] = a.t[i] +carry;
	carry = r.t[i] >> 8;
	r.t[i] &= 0xFF;
    }
    return mk_custom (int64_ops, r);
}
int64_sub = function (a, b) {
    return int64_add (a, int64_neg (b));
}
int64_of_int = function (x) {
    /* assumes exists n, INTEGER_SIZE = (2 ** n)%N */
    var r = new Int64 ();
    r.t = [];
    for (var i = 0;i < INTEGER_SIZE / 8;i++)
	r.t[7 - i] = (x >> (i * 8)) & 0xFF;
    if (x >> (INTEGER_SIZE - 1) == 1)
	for (var i = INTEGER_SIZE / 8 - 1;i < 8;i++)
	    r.t[7 - i] = 0xFF;
    return mk_custom (int64_ops, r);
}
int64_of_bytes = function (bytes) {
    var r = new Int64 ();
    r.t = [];
    for (var i = 0;i < 8;i++)
	r.t[i] = bytes[i];
    return mk_custom (int64_ops, r);
}
int64_of_bytes_le = function (bytes) {
    var r = new Int64 ();
    r.t = [];
    for (var i = 0;i < 8;i++)
	r.t[i] = bytes[7 - i];
    return mk_custom (int64_ops, r);
}
int64_to_bytes = function (a) {
    return a.get (1).t;
}

int64_ops = {
    id : "_j",
    compare : function (a,b) {
	int64_compare (a, b);
    },
    hash : function (a) {
	return 0;
    },
    serialize : function (v, writer) {
	var t = int64_to_bytes (v.get (1));
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
