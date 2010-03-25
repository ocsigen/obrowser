function c() {
}
c.prototype.f = function (s) { 
  alert("test.js -> f(" + s + ")")
};
c.prototype.g = function(s) {
  alert("test.js -> g(" + s + ")");
  this.f(s);
};


function c_(mlo, vm) {
    this.mlo = mlo;
    this.vm = vm;
    c.call (this);
}

c_.prototype.f = function (s) {
  return this.vm.callback_method(this.mlo, "f", [val_string (s)]);
};
c_.prototype.g = function (s) {
  return this.vm.callback_method(this.mlo, "g", [val_string (s)]);
};
c_.prototype.f_ = c.prototype.f;
c_.prototype.g_ = c.prototype.g;

function make_c_ (mlo) {
    return new c_(mlo, this);
}

