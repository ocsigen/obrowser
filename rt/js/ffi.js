/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

// tags

var NO_SCAN_TAG = 251
var FORWARD_TAG = 250
var INFIX_TAG = 249
var OBJECT_TAG = 248
var CLOSURE_TAG = 247
var LAZY_TAG = 246
var ABSTRACT_TAG = 251
var STRING_TAG = 252
var DOUBLE_TAG = 253
var DOUBLE_ARRAY_TAG = 254
var CUSTOM_TAG = 255

// blocks

function is_block (b) { return (b instanceof Block); }
function block_tag (b) { return b.tag; }
function block_size (b) { return b.size; }
function mk_block(size, tag) { return (new Block (size, tag)); }
function field (b, n) { return b.get (n); }
function store_field (b, n, v) { return b.set(n, v); }

// immediates

var UNIT = 0
var FALSE = 0
var TRUE = 1
function mk_bool (v) { return (v?TRUE:FALSE); }
function mk_int (v) { return v; }
function is_long (b) { return (!(b instanceof Block)); }

// floats

function val_float (x)  { return box_float(x); }
function float_val (x)  { return x.get(0); }
function int_of_float (x) { return Math.round (x.get (0)); }

// strings

function val_string (s) { return value_from_string(s); }
function string_val (v) { return string_from_value(v); }

// exceptions

var MAGIC_CAML_EX = 0xEE1664EE
var MAGIC_CAML_CONT = 0xEE1515EE
function caml_catch (e) { if ((e == MAGIC_CAML_CONT) || (e == MAGIC_CAML_EX)) throw (e); }
