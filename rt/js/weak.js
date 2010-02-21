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
function caml_weak_create (size) {
    var a = mk_block (size, ABSTRACT_TAG);
    for (var i = 0;i < size;i++)
	a.set (i, 0);
    register_weak (a);
    return a;
}

// Caml name: set
// Type:      'a t -> int -> 'a option -> unit
function caml_weak_set (a, i, v) {
    a.set (i, v);
    return UNIT;
}

// Caml name: get
// Type:      'a t -> int -> 'a option
function caml_weak_get (a, i) {
    return a.get (i);
}

// Caml name: get_copy
// Type:      'a t -> int -> 'a option
function caml_weak_get_copy (a, i) {
    return a.get (i);
}

// Caml name: check
// Type:      'a t -> int -> bool
function caml_weak_check (a, i) {
    return mk_bool (is_long (a.get (i)));
}
