/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

#include <utils.js>

#define TRAILER_SIZE (4 + 12)
#define EXEC_MAGIC "Caml1999X008"
#define INT32LE(t,i) ((t[i]<<24)|(t[(i)+1]<<16)|(t[(i)+2]<<8)|t[(i)+3])
#define INT32BE(t,i) ((t[(i)+3]<<24)|(t[(i)+2]<<16)|(t[(i)+1]<<8)|t[i])

function as_string (arr) {
    var s = "";
    for (var i = 0;i < arr.length;i++) {
	s += String.fromCharCode (arr[i]);
    }
    return s;
}

function load_program (url) {
    function error (s) {
	throw new Error (s);
    }

    function Buffer (url) {
	/* load file & verify */
	this.text = uudecode (http_get (url, function (e) {throw e;}));
	this.pos = 0;
//	if (as_string(this.text.slice(0,2)) != "#!")
//	    error ("bad bytecode file");

	this.seek_end (-TRAILER_SIZE);
	var trail = this.read (TRAILER_SIZE);
	if (as_string (trail.slice (4,4 + 12)) != EXEC_MAGIC)
	    error ("bad bytecode file (bad magic)");
	this.nsections = INT32LE(trail,0);

	/* read sections */
	this.sections = [];
	this.seek_end (- (TRAILER_SIZE + this.nsections * 8));
	for (var i = 0;i < this.nsections;i++) {
	    var t = this.read (8);
	    this.sections[i] = {
		name : as_string (t.slice (0,4)),
		len : INT32LE(t,4)
	    };
	}
    }
    
    METHODS(Buffer).seek_start = function (i) {
	this.pos = i;
    }
    
    METHODS(Buffer).seek = function (i) {
	this.pos += i;
    }
    
    METHODS(Buffer).seek_end = function (i) {
	this.pos = this.text.length + i;    
    }
    
    METHODS(Buffer).read = function (len) {
	var r = this.text.slice (this.pos, this.pos + len);
	this.pos += len;
	return r;
    }
    
    METHODS(Buffer).seek_section = function (name) {
	var ofs = TRAILER_SIZE + this.nsections * 8;
	for (var i = this.nsections - 1;i >= 0;i--) {
	    ofs += this.sections[i].len;
	    if (this.sections[i].name == name) {
		this.seek_end (-ofs);
		return this.sections[i].len;
	    }
	}
	return -1;
    }
    
    METHODS(Buffer).read_section = function (name) {
	var len = this.seek_section (name);
	if (len == -1) {
	    return null;
	}
	return this.read (len);
    }

    var buf = new Buffer (url);

    /* Load the code */
    var code_text = buf.read_section ("CODE");
    var code = mk_block (code_text.length / 4, 0);
    for (i = 0;i < code_text.length / 4;i++)
	code.set (i, INT32BE(code_text, i * 4));
		      
    /* Build the symbol table */    
    var symbols_text = buf.read_section("PRIM");
    var symbols = [];
    for (var i = 0, s = 0;i < symbols_text.length != 0;i++,s++) {
	symbols[s]= "";
	while (symbols_text[i] != 0)
	    symbols[s] += String.fromCharCode(symbols_text[i++]);
    }
    
    /* Load the globals */
    var data = input_val (buf.read_section ("DATA"), error);

    return {
	code : code,
	symbols : symbols,
	data : data
    };
}
