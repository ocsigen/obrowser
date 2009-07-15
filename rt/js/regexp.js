/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

// Caml name: make
// Type:      string -> string -> t
RT.caml_regexp_make = function (vs, vf) {
    var s = string_from_value (vs);
    var f = string_from_value (vf);
    try { 
	return box_abstract (new RegExp (s, f));
    } catch (e) {
	this.failwith ("Regexp.make: " + e.message);
    }
}

// Caml name: last_index
// Type:      t -> int
RT.caml_regexp_last_index = function (vr) { 
    var r = unbox_abstract (vr) ;
    return r.lastIndex;
}

// Caml name: test
// Type:      t -> string -> bool
RT.caml_regexp_test = function (vr, vs) {
    var r = unbox_abstract (vr) ;
    var s = string_from_value (vs) ;
    return mk_bool (r.test (s));
}

// Caml name: exec
// Type:      t -> string -> string array
RT.caml_regexp_exec = function (vr, vs) {
    var r = unbox_abstract (vr) ;
    var s = string_from_value (vs) ;
    var res = r.exec (s);
    if (res == null) {
	this.raise_constant (NOT_FOUND_EXN);
    } else {
	var vres = mk_block (res.length, 0);
	for (var i = 0;i < res.length;i++) {
	    vres.set (i, value_from_string (res[i]));
	}
	return vres;
    }
}

// Caml name: index
// Type:      t -> string -> int
RT.caml_regexp_index = function (vr, vs) {
    var r = unbox_abstract (vr) ;
    var s = string_from_value (vs) ;
    var i = s.search (r);
    if (i == -1) {
	this.raise_constant (NOT_FOUND_EXN);
    } else {
	return i;
    }
}

// Caml name: replace
// Type:      t -> string -> string -> string
RT.caml_regexp_replace = function (vr, vsub, vs) {
    var r = unbox_abstract (vr) ;
    var s = string_from_value (vs) ;
    var sub = string_from_value (vsub) ;
    return (value_from_string (s.replace (r, sub)));
}

// Caml name: replace_fun
// Type:      t -> (int -> string array -> string) -> string -> string
RT.caml_regexp_replace_fun = function (vr, vf, vs) {
    var r = unbox_abstract (vr) ;
    var s = string_from_value (vs) ;
    var vm = this;
    var f = function () {
	var vargs = mk_block (arguments.length - 2, 0);
	for (var i = 0; i < arguments.length - 2;i++) {
	    vargs.set (i, value_from_string (arguments[i]));
	}
	return string_from_value (vm.callback (vf, [arguments[arguments.length - 2], vargs]));
    }
    return (value_from_string (s.replace (r, f)));
}

// Caml name: split
// Type:      t -> string -> string array
RT.caml_regexp_split = function (vr, vs) {
    var r = unbox_abstract (vr) ;
    var s = string_from_value (vs) ;
    var res = s.split (r);
    var vres = mk_block (res.length, 0);
    for (var i = 0;i < res.length;i++) {
	vres.set (i, value_from_string (res[i]));
    }
    return vres;
}

