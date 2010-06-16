/***********************************************************************/
/*  Int64 implementation in JavaScript                                 */
/*  Some tricks inspired from OCaml's Int64 emulation.                 */
/*                                                                     */
/*  Copyright 2010 Benjamin Canou.                                     */
/*  This file is distributed under the terms of the GNU Library        */
/*  General Public License, version 2 or newer.                        */
/***********************************************************************/

/*** construction from 0, 1 or 2 integers ***/
function Int64 (lo, hi) {
  switch (arguments.length) {
  case 0:
    this.lo = 0;
    this.hi = 0;
    break;
  case 1:
    if (!isInt (lo))
      throw new Error ("Int64.constructor: ill-typed argument");
    this.lo = lo;
    this.hi = lo >> 31;
    break;
  case 2:
    if ((!isInt (lo)) || (!isInt (hi)))
      throw new Error ("Int64.constructor: ill-typed argument");
    this.lo = lo;
    this.hi = hi;
    break;
  default:
    throw new Error ("Int64.constructor: too many arguments");
  }
}

/*** reading and printing ***/
Int64.prototype.toString = function (base, upper) {
  if (base == undefined) base = 10;
  if (base <= 1 || base >= 26)
    throw new Error ("Int64.toString: unbound base");
  var v = this;
  if (this.isZero ()) return "0";
  var b = new Int64 (base);
  var sign = (v.hi < 0);
  var r = "";
  while (!v.isZero ()) {
    var mod = v.mod (b).lo;
    v = v.div (b);
    r = digitString (sign ? -mod : mod, upper) + r;
  }
  if (sign) r = "-" + r;
  return r;
}
function parseInt64 (s) {
    var base = 10, st = 0;
    var r = new Int64();
    if (s[0] == '0' && s[1] == 'x') { base = 16; st = 2; }
    if (s[0] == '0' && s[1] == 'b') { base = 2; st = 2; }
    base = new Int64 (base);
    for (var i = s.length - 1;i >= st;i--) {
	r = r.mul (base);
	r = r.add (new Int64 (stringDigit (s[i])));
    }
    return r;
}

/*** comparison ***/
/* comparison to zero : bool */
Int64.prototype.isZero = function () {
  return (this.lo == 0 && this.hi == 0);
}
/* comparison to another value : Int64 -> bool */
Int64.prototype.compareTo = function (i) {
  if (this.hi == i.hi)
    return (this.lo == i.lo ? 0 : (ult (this.lo, i.lo) ? -1 : 1))
  return ((this.hi < i.hi) ? -1 : 1);
}
Int64.prototype.compareToUnsigned = function (i) {
  if (this.hi == i.hi)
    return (this.lo == i.lo ? 0 : (ult (this.lo, i.lo) ? -1 : 1))
  return (ult (this.hi, i.hi) ? -1 : 1);
}

/*** logical operations ***/
/* bitwise exclusive and : Int64 -> Int64 */
Int64.prototype.and = function (i) {
  var r = new Int64 ();
  r.lo = this.lo & i.lo;
  r.hi = this.hi & i.hi;
  return r;
}
/* bitwise or : Int64 -> Int64 */
Int64.prototype.or = function (i) {
  var r = new Int64 ();
  r.lo = this.lo | i.lo;
  r.hi = this.hi | i.hi;
  return r;
}
/* bitwise exclusive or : Int64 -> Int64 */
Int64.prototype.xor = function (i) {
  var r = new Int64 ();
  r.lo = this.lo ^ i.lo;
  r.hi = this.hi ^ i.hi;
  return r;
}
/* shift left : int -> Int64 */
Int64.prototype.lsl = function (s) {
  var r = new Int64 ();
  if (s == 0) {
    r.lo = this.lo;
    r.hi = this.hi;
  }
  if (s > 0 && s < 32) {
    r.lo = (this.lo << s) & -1;
    r.hi = ((this.hi << s) & -1) | (this.lo >>> (32 - s));        
  }
  if (s >= 32 && s < 64) {
    r.lo = 0;
    r.hi = (this.lo << (s - 32)) & -1;
  }
  return r;
}
/* logical shift right : int -> Int64 */
Int64.prototype.lsr = function (s) {
  var r = new Int64 ();
  if (s == 0) {
    r.lo = this.lo;
    r.hi = this.hi;
  }
  if (s > 0 && s < 32) {
    r.hi = (this.hi >>> s) & -1;
    r.lo = ((this.lo >>> s) & -1) | (this.hi << (32 - s));        
  }
  if (s >= 32 && s < 64) {
    r.hi = 0;
    r.lo = (this.hi >>> (s - 32)) & -1;
  }
  return r;
}
/* arithmetical shift right : int -> Int64 */
Int64.prototype.asr = function (s) {
  var r = new Int64 ();
  if (s == 0) {
    r.lo = this.lo;
    r.hi = this.hi;
  }
  if (s > 0 && s < 32) {
    r.hi = (this.hi >> s) & -1;
    r.lo = ((this.lo >>> s) & -1) | (this.hi << (32 - s));        
  }
  if (s >= 32 && s < 64) {
    r.hi = this.hi >> 31;
    r.lo = (this.hi >> (s - 32)) & -1;
  }
  return r;
}

/*** arithmetic operations ***/
/* unary arithmetic negation : Int64 */
Int64.prototype.neg = function () {
  var r = new Int64 ();
  r.lo = (-this.lo) & -1;
  r.hi = ~this.hi;
  if (r.lo == 0) r.hi = ((r.hi + 1) & -1);
  return r;
}
/* addition : Int64 -> Int64 */
Int64.prototype.add = function (i) {
  var r = new Int64 ();
  r.lo = (this.lo + i.lo) & -1;
  r.hi = (this.hi + i.hi) & -1;
  if (ult (r.lo, this.lo)) r.hi = ((r.hi + 1) & -1);
  return r;
}
/* substraction : Int64 -> Int64 */
Int64.prototype.sub = function (i) {
  var r = new Int64 ();
  r.lo = (this.lo - i.lo) & -1;
  r.hi = (this.hi - i.hi) & -1;
  if (ult (this.lo, i.lo)) r.hi = ((r.hi - 1) & -1);
  return r;
}
/* multiplication : Int64 -> Int64 */
Int64.prototype.mul = function (i) {
  /* we use the following properties :
     - distributivity & associativity of operations
     - the multiplication of two 16-bit words fits in 32 bits
     we decompose the words into [   h   ][ lh | ll ]
     we obtain
       r = (h1 << 32 + lh1 << 16 + ll1) * (h2 << 32 + lh2 << 16 + ll2)
     expanded into
       r = (h1 * h2 << 64) + (h1 << 32) * (lh2 << 16) + (h1 << 32) * ll2
           + (lh1 << 16) * (h2 << 32) + (lh1 << 16) * (lh2 << 16) + (lh1 << 16) * ll2
           + ll1 * (h2 << 32) + ll1 * (lh2 << 16) + ll1 * ll2
    we erase the overflowing product and factor the shifts
      r = (h1 * (lh2 << 16) + (lh1 << 16) * h2 + ll1 * h2 + h1 * ll2 + lh1 * lh2) << 32 (A)
          + (ll1 * lh2 + lh1 * ll2) << 16                                               (B)
          + ll1 * ll2                                                                   (C)
    we obtain two words (A) and (B) for hi and lo, to which we
    add the 16-bit parts of (B), taking care of overflows in the lower
    part of the result. */
  var h1 = this.hi, lh1 = (this.lo >>> 16), ll1 = (this.lo & 0xFFFF);
  var h2 = i.hi, lh2 = (i.lo >>> 16), ll2 = (i.lo & 0xFFFF);
  var A = (((h1 * ((lh2 << 16) & -1)) & -1) + ((((lh1 << 16) & -1) * h2) & -1)
        + ((ll1 * h2) & -1) + ((h1 * ll2) & -1) + lh1 * lh2) & -1;
  var B1 = (ll1 * lh2) & -1, B2 = (lh1 * ll2) & -1;
  var C = ll1 * ll2;
  
  var r = new Int64 ();
  r.hi = A;
  r.hi = (r.hi + (B1 >>> 16)) & -1;
  r.hi = (r.hi + (B2 >>> 16)) & -1;

  r.lo = C;
  var tlo = r.lo + (B1 << 16) & -1;
  if (ult (tlo, r.lo)) r.hi = (r.hi + 1) & -1;
  r.lo = tlo;
  tlo = r.lo + (B2 << 16) & -1;
  if (ult (tlo, r.lo)) r.hi = (r.hi + 1) & -1;
  r.lo = tlo;
  
  return r;
}
/* division : Int64 -> Int64 */
Int64.prototype.div = function (i) {
  var sign = (this.hi < 0) ^ (i.hi < 0);
  var rest = this;
  if (rest.hi < 0) rest = rest.neg ();
  if (i.hi < 0) i = i.neg ();
  if (i.isZero ()) throw new Error ("Int64.div: division by zero");

  /* find N such as i * 2 ** N > this */
  var n = 0;
  while (i.lsl (n).compareToUnsigned (rest) <= 0) n++;
  
  /* reduce multiples of i and 2 ** n, forall 0 <= n < N */
  var quo = new Int64(0);
  while (n >= 0) {
    var shf = i.lsl (n);
    if (rest.compareToUnsigned (shf) >= 0) {
      rest = rest.sub (shf);
      quo = quo.or ((new Int64 (1)).lsl (n));
    }
    n--;
  }
  return sign ? quo.neg () : quo;
}
/* modulo : Int64 -> Int64 */
Int64.prototype.mod = function (i) {
  var sign = (this.hi < 0);
  var rest = this;
  if (rest.hi < 0) rest = rest.neg ();
  if (i.hi < 0) i = i.neg ();
  if (i.isZero ()) throw new Error ("Int64.mod: division by zero");

  /* find N such as i * 2 ** N > this */
  var n = 0;
  while (i.lsl (n).compareToUnsigned (rest) <= 0) n++;
  
  /* reduce multiples of i and 2 ** n, forall 0 <= n < N */
  var quo = new Int64(0);
  while (n >= 0) {
    var shf = i.lsl (n);
    if (rest.compareToUnsigned (shf) >= 0) {
      rest = rest.sub (shf);
    }
    n--;
  }
  return sign ? rest.neg () : rest;
}

/*** utilities ***/
function isInt(v) {
  return (typeof (v) == 'number' && (!isNaN (v)) && ((v & -1) === v))
}
function ult (a,b) {
    return ((a >= 0) ? ((b < 0) || (a < b)) : ((b < 0) && (a < b)));
}
var _lowerCaseDigits = [
  '0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','g','h','i',
  'j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'];
var _upperCaseDigits = [
  '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I',
  'J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'];
var _digitsReversed = {}
for (i in _lowerCaseDigits) _digitsReversed[_lowerCaseDigits[i]] = i;
for (i in _upperCaseDigits) _digitsReversed[_upperCaseDigits[i]] = i;
function digitString (v, u) {
  var s = u ? _lowerCaseDigits[v] : _upperCaseDigits[v];
  if (s == undefined)
    throw new Error ("digitString: unbound number " + v);
  return s;
}
function stringDigit (v) {
  var s = _digitsReversed[v];
  if (s == undefined)
      throw new Error ("stringDigit: unbound digit " + v);
  return s;
}

