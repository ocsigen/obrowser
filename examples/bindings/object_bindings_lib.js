function number (init) {
    this.v = init;
}

number.prototype.get = function () {
    return this.v;
}

number.prototype.incr = function (n) {
    this.v = this.v + n;
}

number.prototype.print = function () {
    window.alert (this.to_string ());
}

number.prototype.to_string = function () {
    return ("<" + this.v + ">")
}

function calculator (init) {
    this.value = init;
}

calculator.prototype.add = function (v) {
    this.value.incr (v.get ());
}

calculator.prototype.result = function () {
    return this.value;
}

calculator.prototype.print = function () {
    return this.value.print ();
}
