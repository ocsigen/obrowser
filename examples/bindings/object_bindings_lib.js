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
