function store () { this.cur = 0; }
store.prototype.add = function (v) { this[++this.cur] = v; return this.cur; }
store.prototype.get = function (id) { return this[id]; }

function number (init, name) { this.v = init; this.name = name; }
number.prototype.get = function () { return this.v; }
number.prototype.dup = function () { return new number (this.v); }
number.prototype.incr = function (n) { this.v = this.v + n; }
number.prototype.print = function () { window.alert (this.to_string ()); }
number.prototype.to_string = function () { return (this.name + "<" + this.v + ">") }

function Calculator (init) { this.value = init; }
Calculator.prototype.add = function (v) { this.value.incr (v.get ());}
Calculator.prototype.result = function () { return this.value;}
Calculator.prototype.print = function () { return this.value.print ();}

function SuperCalculator (message) { this.message = message ; Calculator.call (this, new number (0, "super_internal_storage")); }
/* inheritance : */
for (i in Calculator.prototype) SuperCalculator.prototype[i] = Calculator.prototype[i];
SuperCalculator.prototype.alert = function () { window.alert (this.message + " " + this.value.to_string ()); }

function Stack () { this.items = new Array (); }
Stack.prototype.push = function (s) { this.items.push (s); }
Stack.prototype.get_all = function () { return this.items; }
Stack.prototype.reinit = function (v) { this.items = v; }
Stack.prototype.disp = function () { console.debug ("--- disp ---"); return UNIT; }
