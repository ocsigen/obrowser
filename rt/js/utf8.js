/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

function encode_utf8 (s) {
    var v = mk_block (0, STRING_TAG);
    var vi = 0;
    for (var i = 0; i < s.length; i++) {
	var c = s.charCodeAt (i);
	if (c < 0x80) {
	    /* one byte (ascii) */
	    v.size++;
	    v.set (vi++, c);
	} else {
	    if(c < 0x800) {
		/* two bytes */
		v.size += 2;
		v.set (vi++, (c >> 0x06) | 0xC0);
		v.set (vi++, (c & 0x3F) | 0x80);
	    } else {
		/* three bytes */
		v.set (vi++, (c >> 12) | 0xE0);
		v.set (vi++, ((c >> 6) & 0x3F) | 0x80);
		v.set (vi++, (c & 0x3F) | 0x80);
	    }
	}
    }
    v.size++;
    v.set (vi, 0);
    return v;
}

function decode_utf8 (v) {
    var s = "";
    for (var i = 0;i < v.size - 1;) {
	var c1 = v.get (i++);
	if (c1 < 0x80) {
	    s += String.fromCharCode(c1);
	} else {
	    if((c1 >= 0xC0) && (c1 < 0xE0)) {
		var c2 = v.get (i++);
		s += String.fromCharCode (((c1 & 0x1F) << 6) | (c2 & 0x3F));
	    } else {
		var c2 = v.get (i++);
		var c3 = v.get (i++);
		s += String.fromCharCode(((c1 & 0xF) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F));
	    }
	}
    }
    return s;
}
