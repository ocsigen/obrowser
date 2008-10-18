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
