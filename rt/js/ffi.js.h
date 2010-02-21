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

#define is_block(b)         (b instanceof Block)
#define block_tag(b)        (b).tag
#define block_size(b)       (b).size
#define mk_block(size, tag) (new Block (size, tag))
#define field(b,n)          (b).get(n)
#define store_field(b,n,v)  (b).set(n,v)

// immediates

#define UNIT       0
#define FALSE      0
#define TRUE       1
#define mk_bool(v) (v?TRUE:FALSE)
#define is_long(b) (!(b instanceof Block))

// floats

#define value_from_float(x)  box_float(x)
#define float_from_value(x)  x.get(0)
#define int_of_float(x)      Math.round (x.get (0))

// strings

#define value_from_string(s) value_from_string(s)
#define string_from_value(v) string_from_value(v)

// exceptions

#define MAGIC_CAML_EX   0xEE1664EE
#define MAGIC_CAML_CONT 0xEE1515EE
#define caml_catch(e) if (((e) == MAGIC_CAML_CONT) || ((e) == MAGIC_CAML_EX)) throw (e)
