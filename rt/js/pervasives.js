/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  STRINGS                                                                  //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

// Caml name: is_printable
// Type:      char -> bool
RT.caml_is_printable = function (c) {
    return mk_bool (c >= 0x20);
}

// Caml name: unsafe_fill
// Type:      string -> int -> int -> char -> unit
RT.caml_fill_string = function (s, st, len, c) {
    for (var i = 0;i < len;i++)
	s.set (st + i, c);
    return UNIT;
}

// Caml name: string_create
// Type:      int -> string
RT.caml_create_string = function (len) {
    var b = mk_block (len + 1, STRING_TAG);
    for (var i = 0;i <= len;i++)
	b.set (i, 0);
    return b;
}

// Caml name: string_blit
// Type:      string -> int -> string -> int -> int -> unit
RT.caml_blit_string = function (s1, ofs1, s2, ofs2, n) {
    for (var i = 0;i < n;i++) {
	s2.set (ofs2 + i, s1.get (ofs1 + i));
    }
    return UNIT;
}

// Caml name: get
// Type:      string -> int -> char
RT.caml_string_get = function (arr, idx) {
    if (idx >= 0 && idx < arr.size - 1) {
	return arr.get(idx);
    }
    this.array_bound_error ();
}

// Caml name: unsafe_get
// Type:      string -> int -> char
RT.caml_string_unsafe_get = function (arr, idx) {
    return arr.get(idx);
}

// Caml name: set
// Type:      string -> int -> char -> unit
RT.caml_string_set = function (arr, idx, val) {
    if (idx >= 0 && idx < arr.size - 1) {
	arr.set(idx,val);
	return UNIT;
    }
    this.array_bound_error ();
}

// Caml name: unsafe_set
// Type:      string -> int -> char -> unit
RT.caml_string_unsafe_set = function (arr, idx, val) {
    arr.set(idx,val);
    return UNIT;
}

// Caml name: string_length
// Type:      string -> int
RT.caml_ml_string_length = function (s) {
    return (s.size - 1);
}

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  ARRAYS                                                                   //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

// Caml name: get
// Type:      'a array -> int -> 'a
RT.caml_array_get = 
RT.caml_array_get_addr = 
RT.caml_array_get_float = function (arr, idx) {
    if (idx >= 0 && idx < arr.size) {
	return arr.get(idx);
    }
    this.array_bound_error ();
}

// Caml name: set
// Type:      'a array -> int -> 'a -> unit
RT.caml_array_set = 
RT.caml_array_set_addr = 
RT.caml_array_set_float = function (arr, idx, val) {
    if (idx >= 0 && idx < arr.size) {
	arr.set(idx,val);
	return UNIT;
    }
    this.array_bound_error ();
}

// Caml name: unsafe_get
// Type:      'a array -> int -> 'a
RT.caml_array_unsafe_get = 
RT.caml_array_unsafe_get_addr = 
RT.caml_array_unsafe_get_float = function (arr, idx) {
    return arr.get(idx);
}

// Caml name: unsafe_set
// Type:      'a array -> int -> 'a -> unit
RT.caml_array_unsafe_set = 
RT.caml_array_unsafe_set_addr = 
RT.caml_array_unsafe_set_float = function (arr, idx, val) {
    arr.set(idx,val);
    return UNIT;
}

// Caml name: make
// Type:      int -> 'a -> 'a array
RT.caml_make_vect = function (len, init) {
    var b = mk_block (len, 0);
    for (var i = 0;i < len;i++) {
	b.set(i,init);
    }
    return b;
}

// Caml name: make_array
// Type:      'a array -> 'a array
RT.caml_make_array = function (init) {
    var b = mk_block (init.size, 0);
    for (var i = 0;i < init.size;i++) {
	b.set(i,init.get (i));
    }
    return b;
}

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  COMPARE                                                                  //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

function caml_compare (a,b) {
    if (a == b)	return 0;
    if (!is_block(a)) {
	if (is_block(b)) {
	    if (b.tag == FORWARD_TAG)
		return caml_compare (a, b.get(0));
	    return -1;
	}
	return a - b;
    }
    if (!is_block(b)) {
	if (a.tag == FORWARD_TAG)
	    return caml_compare (a.get(0), b);
	return 1;
    }
    // TODO: verify closures
    if (a.tag == CLOSURE_TAG || a.tag == INFIX_TAG)
	this.raise_with_string (INVALID_EXN, "equal: functional value");
    if (a.tag != b.tag)
	return a.tag - b.tag;
    if (a.tag == CUSTOM_TAG)
	return a.get (0).compare (a, b);
    if (a.tag == DOUBLE_TAG) {
	a = unbox_float (a);
	b = unbox_float (b);
	if (a == b) return 0;
	return ((a < b) ? -1 : 1);
    }
    if (a.size != b.size)
	return a.size - b.size;
    for (var i = 0;i < a.size;i++) {
	t = caml_compare (a.get(i), b.get(i));
	if (t != 0) return t;
    }
    return 0;
}


// Caml name: compare
// Type:      'a -> 'a -> int
RT.caml_string_compare =
RT.caml_compare = function (a, b) {
    return caml_compare (a,b);
}

// Caml name: (=)
// Type:      'a -> 'a -> bool
RT.caml_equal = function (a, b) {
    return mk_bool (caml_compare (a,b) == 0);
}

// Caml name: (<>)
// Type:      'a -> 'a -> bool
RT.caml_notequal = function (a, b) {
    return mk_bool (caml_compare (a,b) != 0);
}

// Caml name: (=)
// Type:      'a -> 'a -> bool
RT.caml_string_equal = function (a, b) {
    return mk_bool (caml_compare (a,b) == 0);
}

// Caml name: (<>)
// Type:      'a -> 'a -> bool
RT.caml_string_notequal = function (a, b) {
    return mk_bool (caml_compare (a,b) != 0);
}

// Caml name: (<)
// Type:      'a -> 'a -> bool
RT.caml_lessthan =
RT.caml_lt_float = function (a, b) {
    return mk_bool (caml_compare (a,b) < 0);
}

// Caml name: (<=)
// Type:      'a -> 'a -> bool
RT.caml_lessequal =
RT.caml_le_float = function (a, b) {
    return mk_bool (caml_compare (a,b) <= 0);
}

// Caml name: (>)
// Type:      'a -> 'a -> bool
RT.caml_greaterthan =
RT.caml_gt_float = function (a, b) {
    return mk_bool (caml_compare (a,b) > 0);
}

// Caml name: (>=)
// Type:      'a -> 'a -> bool
RT.caml_greaterequal =
RT.caml_ge_float = function (a, b) {
    return mk_bool (caml_compare (a,b) >= 0);
}

// Caml name: (=)
// Type:      'a -> 'a -> bool
RT.caml_eq_float = function (a, b) {
    return mk_bool (caml_compare (a,b) == 0);
}

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  HASH                                                                     //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

function caml_hash_univ_param (count, limit, obj) {
    hash_univ_limit = limit;
    hash_univ_count = count;
    hash_accu = 0;
    hash_aux (obj);
    return (hash_accu & 0x3FFFFFFF);
}

#define ALPHA 65599
#define BETA  19
#define COMBINE(n)  (hash_accu = hash_accu * ALPHA + (n))
#define COMBINE_SMALL(n) (hash_accu = hash_accu * BETA + (n))

function hash_aux(obj){
    hash_univ_limit--;
    if (hash_univ_count < 0 || hash_univ_limit < 0) return;
    
    if (is_long(obj)) {
	hash_univ_count--;
	COMBINE(Long_val(obj));
	return;
    }
    
    switch (obj.tag) {
    case STRING_TAG: {
	hash_univ_count--;
	for (var p = 0;p < obj.size - 1; p++)
            COMBINE_SMALL(obj.get (p));
	break;
    }
    case DOUBLE_TAG: {
	hash_univ_count--;
	var bytes = bytes_of_float (unbox_float (obj));
	for (var p = 7; p >= 0; p--)
            COMBINE_SMALL(bytes[p]);
	break;
    }
    case DOUBLE_ARRAY_TAG: {
	hash_univ_count--;
	for (var j = 0; j < obj.size; j++) {
	    var bytes = bytes_of_float (unbox_float (obj.get (j)));
	for (var p = 7; p >= 0; p--)
            COMBINE_SMALL(bytes[p]);
	}
	break;
    }
    case ABSTRACT_TAG:
      break;
    case INFIX_TAG:
	// not sure but eh, CLOSUREREC is quite implementation dependent !
	hash_aux(obj.shift (-obj.offset));
      break;
    case FORWARD_TAG:
	hash_univ_limit++; //no goto in js
	hash_aux (obj.get (0));
	break;
    case OBJECT_TAG:
	hash_univ_count--;
	COMBINE(obj.get (1));
	break;
    case CUSTOM_TAG:
	if (obj.get (0).hash != null) {
            hash_univ_count--;
            COMBINE(obj.get (0).hash (obj));
	}
	break;
    default: {
	hash_univ_count--;
	COMBINE_SMALL(obj.tag);
	var i = obj.size;
	while (i != 0) {
            i--;
            hash_aux(obj.get (i));
	}
	break;
    }
    }
    return;
}

// Caml name: hash_param
// Type:      int -> int -> 'a -> int
RT.caml_hash_univ_param = function (count, limit, obj) {
    return caml_hash_univ_param (count, limit, obj);
}

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  CASTS                                                                    //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
// Caml name: format_int
// Type:      string -> int -> string
RT.caml_format_int = function (fmt, x) {
    var fmt = string_from_value (fmt);
    var t = fmt[fmt.length - 1];
    var n = Number (x).toString (t == 'd' ? 10 : (t == 'o' ? 8 : (t == 'x' ? 16 : 10 /* err */)));
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

// Caml name: format_float
// Type:      string -> float -> string
RT.caml_format_float = function (fmt, x) {
    /* format unused */
    return value_from_string (unbox_float (x).toString (10));
}

// Caml name: int_of_string
// Type:      string -> int
RT.caml_int_of_string = function (s) {
    var res = parseInt (string_from_value (s));
    if (isNaN (res)) {
	this.failwith ("int_of_string");
    } else {
	return res ;
    }
}

// Caml name: float_of_string
// Type:      string -> int
RT.caml_float_of_string = function (s) {
    return box_float (parseFloat (string_from_value (s)));
}

// Caml name: int_of_float
// Type:      float -> int
RT.caml_int_of_float = function (x) {
    return int_of_float (x);
}

// Caml name: float_of_int, float
// Type:      int -> float
RT.caml_float_of_int = function (x) {
    return float_of_int (x);
}

// Caml name: float_of_bits
// Type:      int64 -> float
RT.caml_int64_float_of_bits = function (i) {
    return float_of_bytes (int64_to_bytes (i));
}

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  OPERATORS                                                                //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

// Caml name: ( *. )
// Type:      float -> float -> float
RT.caml_mul_float = function (a, b) {
    return box_float (unbox_float (a) * unbox_float (b));
}

// Caml name: ( /. )
// Type:      float -> float -> float
RT.caml_div_float = function (a, b) {
    return box_float (unbox_float (a) / unbox_float (b));
}

// Caml name: (~-.)
// Type:      float -> float
RT.caml_neg_float = function (a) {
    return box_float (- unbox_float (b));
}
// Caml name: (+.)
// Type:      float -> float -> float
RT.caml_add_float = function (a, b) {
    return box_float (unbox_float (a) + unbox_float (b));
}
// Caml name: (-.)
// Type:      float -> float -> float
RT.caml_sub_float = function (a, b) {
    return box_float (unbox_float (a) - unbox_float (b));
}

// Caml name: ( ** )
// Type:      float -> float -> float
RT.caml_power_float =
RT.pow = function (a, b) {
    return box_float (Math.pow (unbox_float (a), unbox_float (b)));
}

// Caml name: exp
// Type:      float -> float
RT.caml_exp_float =
RT.exp = function (a, b) {
    return box_float (Math.exp (unbox_float (a), unbox_float (b)));
}

// Caml name: acos
// Type:      float -> float
RT.caml_acos_float =
RT.acos = function (x) {
    return box_float (Math.acos (unbox_float (x)));
}

// Caml name: asin
// Type:      float -> float
RT.caml_asin_float =
RT.asin = function (x) {
    return box_float (Math.asin (unbox_float (x)));
}

// Caml name: atan
// Type:      float -> float
RT.caml_atan_float =
RT.atan = function (x) {
    return box_float (Math.atan (unbox_float (x)));
}

// Caml name: atan2
// Type:      float -> float -> float
RT.caml_atan2_float =
RT.atan2 = function (a, b) {
    return box_float (Math.atan2 (unbox_float (a), unbox_float (b)));
}

// Caml name: cos
// Type:      float -> float
RT.caml_cos_float =
RT.cos = function (x) {
    return box_float (Math.cos (unbox_float (x)));
}

// Caml name: cosh
// Type:      float -> float
RT.caml_cosh_float =
RT.cosh = function (x) {
    x = unbox_float (x);
    return box_float ((Math.exp (x) + Math.exp (-x)) / 2);
}

// Caml name: log
// Type:      float -> float
RT.caml_log_float =
RT.log = function (x) {
    return box_float (Math.log (unbox_float (x)));
}

// Caml name: log10
// Type:      float -> float
RT.caml_log10_float =
RT.log10 = function () {
    return box_float (Math.log (unbox_float (x)) / Math.log (10));
}

// Caml name: sin
// Type:      float -> float
RT.caml_sin_float =
RT.sin = function (x) {
    return box_float (Math.sin (unbox_float (x)));
}

// Caml name: sinh
// Type:      float -> float
RT.caml_sinh_float =
RT.sinh = function (x) {
    x = unbox_float (x);
    return box_float ((Math.exp (x) - Math.exp (-x)) / 2);
}

// Caml name: sqrt
// Type:      float -> float
RT.caml_sqrt_float =
RT.sqrt = function (x) {
    return box_float (Math.sqrt (unbox_float (x)));
}

// Caml name: tan
// Type:      float -> float
RT.caml_tan_float =
RT.tan = function (x) {
    return box_float (Math.tan (unbox_float (x)));
}

// Caml name: tanh
// Type:      float -> float
RT.caml_tanh_float =
RT.tanh = function (x) {
    return box_float (Math.tanh (unbox_float (x)));
}

// Caml name: ceil
// Type:      float -> float
RT.caml_ceil_float =
RT.ceil = function (x) {
    return box_float (Math.ceil (unbox_float (x)));
}

// Caml name: floor
// Type:      float -> float
RT.caml_floor_float =
RT.floor = function (x) {
    return box_float (Math.floor (unbox_float (x)));
}

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  NOT FOR THE CASUAL USER ;-)                                              //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

var named_values = [];
#define caml_named_value(vname) named_values[string_from_value (vname)]

// Caml name: register_named_value
// Type:      string -> 'a -> unit
RT.caml_register_named_value = function (vname, val) {
    named_values[string_from_value (vname)] = val;
    return UNIT;
}

// Type:      int -> unit
RT.caml_ensure_stack_capacity = function (required_space) {
    return UNIT;
}

// Caml name: is_block
// Type:      t -> bool
RT.caml_obj_is_block = function (x) {
    return mk_bool (is_block (x));
}
// Caml name: is_int
// Type:      t -> bool
RT.caml_obj_is_int = function (x) {
    return mk_bool (is_long (x));
}
// Caml name: tag
// Type:      t -> int
RT.caml_obj_tag = function (x) {
    return x.tag;
}
// Caml name: set_tag
// Type:      t -> int -> unit
RT.caml_obj_set_tag = function (x, tag) {
    x.tag = tag;
    return UNIT;
}
// Caml name: size
// Type:      t -> int
RT.caml_obj_size = function (x) {
    return x.size;
}
// Caml name: field
// Type:      t -> int -> t
RT.caml_obj_field = function (x, i) {
    return x.get (i);
}
// Caml name: set_field
// Type:      t -> int -> t -> unit
RT.caml_obj_set_field = function (x, i, v) {
    x.set (i, v);
    return UNIT;
}
// Caml name: new_block
// Type:      int -> int -> t
RT.caml_obj_block = function (tag, size) {
    var b = mk_block (size, tag);
    return b;
}

// Caml name: dup
// Type:      t -> t
RT.caml_obj_dup = function (v) {
    if (is_long (v))
	return v;
    var b = mk_block (v.size, v.tag);
    for (var i = 0;i < v.size;i++)
	b.set (i, v.get (i));
    return b;
}

// Caml name: truncate
// Type:      t -> int -> unit
RT.caml_obj_truncate = function (x, size) {
    x.size = size;
    return UNIT;
}

// Caml name: ldexp
// Type:      float -> int -> float
RT.caml_ldexp_float = function (m, e) {
    return box_float (m * Math.pow (2, unbox_float (e)));
}

// Caml name: frexp
// Type:      float -> float * int
RT.caml_frexp_float = function (v) {
    var x = unbox_float (v);
    var e = Math.ceil (Math.log (Math.abs (x)) / Math.log (2));
    return pair (box_float (x * Math.pow (2, -e)), e);
}

RT.caml_alloc_dummy = function (size) {
    return mk_block (size, 0);
}

RT.caml_update_dummy = function (dummy, newval) {
//    if (!is_block(dummy) || !is_block(newval) || dummy.size != newval.size) {
//	this.failwith ("caml_update_dummy");
//    }
    for (var i = 0;i < dummy.size;i++) {
	dummy.set (i, newval.get (i));
    }
    return UNIT;
}

/*
// Caml name: abs_float
// Type:      float -> float
RT.%absfloat = function () {
  throw new Error ("%absfloat" + " not implemented");
}

// Caml name: mod_float
// Type:      float -> float -> float
RT.caml_fmod_float =
RT.fmod = function () {
  throw new Error ("caml_fmod_float" + " not implemented");
}

// Caml name: modf
// Type:      float -> float * float
RT.caml_modf_float = function () {
  throw new Error ("caml_modf_float" + " not implemented");
}

// Caml name: classify_float
// Type:      float -> fpclass
RT.caml_classify_float = function () {
  throw new Error ("caml_classify_float" + " not implemented");
}

// Caml name: sys_exit
// Type:      int -> 'a
RT.caml_sys_exit = function () {
  throw new Error ("caml_sys_exit" + " not implemented");
}


"%greaterequal",
"caml_greaterequal"
"caml_string_greaterequal"

"%compare",
"caml_compare"
"caml_int_compare"
"caml_float_compare"
"caml_string_compare"
"caml_nativeint_compare"
"caml_int32_compare"
"caml_int64_compare"

*/

