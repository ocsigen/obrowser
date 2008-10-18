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
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  UTILITIES                                                                //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

#ifndef __UTILS_H
#define __UTILS_H

// \in operator
function is_in (v, a) {
    for (var i in a) {
	if (v == a[i])
	    return true;
    }
    return false;
}

// index of v in a
function index_in (v, a) {
    for (var i in a) {
	if (v == a[i])
	    return i;
    }
    return -1;
}

// sprintf
function sprintf () {
    if ((arguments.length >= 1) && (typeof arguments[0] == "string")) {
	var narg = 1;
	var format = arguments[0];
	var res = "";
	var i = 0;
	while (i < format.length) {
	    if (format[i] == '%') {
		i++;
		if (i >= format.length) {
		    throw new Error ("printf: bad format");
		} else {
		    if (format[i] == '%') {
			res += "%";
			i++;
			break;
		    } else {
			var pad = ' ';
			if (!is_in(format[i], ['d','f','x','X','r'])) {
			    pad = format[i++];
			    if (i >= format.length)
				throw new Error ("printf: bad format");
			}
			var len = 0;
			var nums = ['0','1','2','3','4','5','6','7','8','9'];
			while (is_in(format[i], nums)) {
			    len = len * 10 + index_in (format[i++], nums);
			    if (i >= format.length)
				throw new Error ("printf: bad format");
			}
			if (narg >= arguments.length)
			    throw new Error ("printf: bad format");
			var fmt;
			switch (format[i++]) {
			case 'r': {
			    var m = 10;
			    if (i < format.length) {
				if (format[i] == '{')
				    i++;
				if (i >= format.length)
				    throw new Error ("printf: bad format");
				while (format[i] != '}') {
				    m = m * 10 + index_in (format[i++], nums);
				    if (i >= format.length)
					throw new Error ("printf: bad format");
				}
			    }
			    fmt = repr (arguments[narg++], m);
			    break;
			}
			case 'd' :
			case 'f' :
			    fmt =
				Number (arguments[narg++])
			    .toString (10);
			    break;
			case 'x':
			    fmt =
				Number (arguments[narg++])
			    .toString (16).toUpperCase ();
			    break;
			case 'X':
			    fmt= 
				Number (arguments[narg++])
			    .toString (16).toUpperCase ();
			    break;
			default :
			    throw new Error ("printf: bad format");
			}
			for (var j = 0;j < len - fmt.length;j++)
			    res += pad;
			res += fmt;
		    }
		}
	    } else {
		res += format[i++];
	    }
	}
    } else {
	throw new Error ("printf: bad format");
    }
    return res;
}

// gets a char from an event's keyCode (approximative)
function char_from_event (e) {
    if (e.charCode)
	return e.charCode;
    var c = e.keyCode;
    if (e.shiftKey)
	return e.keyCode;
    if (e.keyCode >= e.DOM_VK_A && e.keyCode <= e.DOM_VK_Z)
	return e.keyCode + ("a".charCodeAt(0) - "A".charCodeAt(0));
}

// store (key/value) pair
function set_cookie ( name, value) {
    document.cookie = name + "=" + escape (value);
}

// retrieve value from a key (or null)
function get_cookie (name) {
    if (document.cookie) {
        var i = document.cookie.indexOf(name);
        if ( i != -1) {
            var s = (document.cookie.indexOf( "=", i) + 1);
            var e = document.cookie.indexOf( ";", i);
            if (e == -1) e = document.cookie.length;
            return unescape(document.cookie.slice(s, e));
        }
    }
    return null;
}

// make 'daughter' inherit 'mother' by copying its prototype
// use this.mother_constructor to acces super constructor
// use this.base.x to acces mother's x field
function make_inherit (daughter, mother) {
    function tmp() {};
    tmp.prototype = mother.prototype;
    daughter.prototype = new tmp();
    daughter.prototype.constructor = daughter;
    daughter.prototype.mother_constructor = mother;
    if (mother.base)
	mother.prototype.base = mother.base;
    daughter.base = mother.prototype;
}

// get text file from url as string
function http_get (url, error) {
    var xmlhttp=false;
    if (ie) {
        try {
	    xmlhttp = new ActiveXObject("Msxml2.XMLHTTP");
        } catch (e) {
	    try {
		xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
	    } catch (E) {
		throw new Error ("Unsupported Internet Explorer");
	    }
        }
    } else {
	if (typeof XMLHttpRequest != 'undefined')
	    xmlhttp = new XMLHttpRequest();
	else
	    throw new Error ("Unsupported Browser");
    }
    try {
	xmlhttp.open("GET", url, false);
	xmlhttp.send(null);
    } catch (e) {
	throw new Error ("unable to load file " + url + ": " + e.message);
    }
    return xmlhttp.responseText;
}

// get text file from url with post parameters as string
function http_post (url, post, error) {
    var xmlhttp=false;
    /*@cc_on @*/
    /*@if (@_jscript_version >= 5)
        try {
          xmlhttp = new ActiveXObject("Msxml2.XMLHTTP");
        } catch (e) {
          try {
            xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
          } catch (E) {
            xmlhttp = false;
          }
        }
      @else
        xmlhttp = false;
      @end @*/
    if (!xmlhttp && typeof XMLHttpRequest != 'undefined') {
	try {
   	    xmlhttp = new XMLHttpRequest();
	    xmlhttp.open("POST", url, false);
	    xmlhttp.send(post);
	} catch (e) {
	    throw new Error ("unable to load file " + url + ": " + e.message);
 	}
//	if (xmlhttp.status != 200) {
//	    var data = uudecode (xmlhttp.responseText);
//	    error (input_val (data, caml_failwith));
//	}
	return xmlhttp.responseText;
    }
    throw new Error ("unable to load file");
}

// uudecode a string to a byte (int) array
function uudecode (s) {
    var out = new Array ();
    var n = 0;
    var as = s.split ("\n");
    for (var i = 1;i < as.length;i++) {
	if (as[i].length > 1 && as[i] != "end") {
	    var len = as[i].charCodeAt(0) - 0x20;
	    var k = 0;
	    for (var j = 0;j < len / 3;j++) {
		var b0 = as[i].charCodeAt(1 + j * 4 + 0),
		    b1 = as[i].charCodeAt(1 + j * 4 + 1),
		    b2 = as[i].charCodeAt(1 + j * 4 + 2),
		    b3 = as[i].charCodeAt(1 + j * 4 + 3);
		if (k++ < len) out[n++] =
		    (((b0 - 0x20) & 0x3F) << 2 & 0xFC)
		    | (((b1 - 0x20) & 0x3F) >> 4 & 0x03);            
		if (k++ < len) out[n++] =
		    (((b1 - 0x20) & 0x3F) << 4 & 0xF0)
		    | (((b2 - 0x20) & 0x3F) >> 2 & 0x0F);            
		if (k++ < len) out[n++] =
		    (((b2 - 0x20) & 0x3F) << 6 & 0xC0)
		    |  ((b3 - 0x20) & 0x3F);
	    }
	}
    }
    return out;
}

// unsigned operator <
function ult (a,b) {
    return ((a >= 0) ? ((b < 0) || (a < b)) : ((b < 0) && (a > b)));
}

#endif /*__UTILS_H*/
