/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

var OUT_OF_MEMORY_EXN = 0;               /* "Out_of_memory" */
var SYS_ERROR_EXN = 1;                   /* "Sys_error" */
var FAILURE_EXN = 2;                     /* "Failure" */
var INVALID_EXN = 3;                     /* "Invalid_argument" */
var END_OF_FILE_EXN = 4;                 /* "End_of_file" */
var ZERO_DIVIDE_EXN = 5;                 /* "Division_by_zero" */
var NOT_FOUND_EXN = 6;                   /* "Not_found" */
var MATCH_FAILURE_EXN = 7;               /* "Match_failure" */
var STACK_OVERFLOW_EXN = 8;              /* "Stack_overflow" */
var SYS_BLOCKED_IO = 9;                  /* "Sys_blocked_io" */
var ASSERT_FAILURE_EXN = 10;             /* "Assert_failure" */
var UNDEFINED_RECURSIVE_MODULE_EXN = 11; /* "Undefined_recursive_module" */

VM.prototype.raise_constant = function(tag) {
    var b = mk_block (1,0);
    b.set(0, this.data.get (tag));
    this.raise (b);
}

VM.prototype.raise_with_arg = function (tag,val) {
    var b = mk_block (2,0);
    b.set(0,this.data.get (tag));
    b.set(1,val);
    this.raise (b);
}

VM.prototype.raise_with_string = function (tag, msg) {
    var b = mk_block (2,0);
    b.set(0,this.data.get (tag));
    b.set(1,value_from_string (msg));
    this.raise (b);
}

VM.prototype.invalid_arg = function (msg) {
    this.raise_with_string (INVALID_EXN, msg);
}

VM.prototype.failwith = function (msg) {
    this.raise_with_string (FAILURE_EXN, msg);
}

VM.prototype.array_bound_error = function () {
    this.invalid_arg ("index out of bounds");
}

VM.prototype.raise_end_of_file = function () {
    this.raise_constant (END_OF_FILE_EXN);
}
