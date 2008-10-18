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
//  RUNTIME :: CUSTOMS                                                      //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

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
	return reader.read32u ();
    }
};

register_custom (int32_ops);

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
	throw new Error ("int32 not supported yet");
    },
    deserialize : function (reader) {
	var l = reader.read8u ();
	if (l == 1)
	    return reader.read32u ();
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
