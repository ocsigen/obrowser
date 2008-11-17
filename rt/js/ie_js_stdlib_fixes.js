/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

var ie = (navigator.appName == "Microsoft Internet Explorer") ;

#define METHODS(c) c.prototype

if (Array.map == null) {
    Array.map = function (a,f) {
	if (arguments.length == 1) {
	    f = a;
	    a = this;
	}
	var res = new Array ();
	for (i in a) {
	    res[i] = f(a[i]);
	}
	return res;
    }
}
