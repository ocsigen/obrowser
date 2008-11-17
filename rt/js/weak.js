/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

// Idea : use timers to clear weak values at regular intervals.
// Gorgeous.
// But I don not think there is any other option.

var registered_weaks = null;
function register_weak (a) {
    registered_weaks = {
	car : a,
	cdr : registered_weaks
    };
}

// Caml name: create
// Type:      int -> 'a t
RT["caml_weak_create"] = function (size) {
    var a = mk_block (size, ABSTRACT_TAG);
    for (var i = 0;i < size;i++)
	a.set (i, 0);
    register_weak (a);
    return a;
}

// Caml name: set
// Type:      'a t -> int -> 'a option -> unit
RT["caml_weak_set"] = function (a, i, v) {
    a.set (i, v);
    return UNIT;
}

// Caml name: get
// Type:      'a t -> int -> 'a option
RT["caml_weak_get"] = function (a, i) {
    return a.get (i);
}

// Caml name: get_copy
// Type:      'a t -> int -> 'a option
RT["caml_weak_get_copy"] = function (a, i) {
    return a.get (i);
}

// Caml name: check
// Type:      'a t -> int -> bool
RT["caml_weak_check"] = function (a, i) {
    return mk_bool (is_long (a.get (i)));
}
