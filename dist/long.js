(function (global, factory) {
    typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports) :
    typeof define === 'function' && define.amd ? define(['exports'], factory) :
    (global = typeof globalThis !== 'undefined' ? globalThis : global || self, factory(global.Long = {}));
}(this, (function (exports) { 'use strict';

    var wasm = null;
    try {
        wasm = new WebAssembly.Instance(new WebAssembly.Module(new Uint8Array([
            0, 97, 115, 109, 1, 0, 0, 0, 1, 13, 2, 96, 0, 1, 127, 96, 4, 127, 127, 127, 127, 1, 127, 3, 7, 6, 0, 1, 1, 1, 1, 1, 6, 6, 1, 127, 1, 65, 0, 11, 7, 50, 6, 3, 109, 117, 108, 0, 1, 5, 100, 105, 118, 95, 115, 0, 2, 5, 100, 105, 118, 95, 117, 0, 3, 5, 114, 101, 109, 95, 115, 0, 4, 5, 114, 101, 109, 95, 117, 0, 5, 8, 103, 101, 116, 95, 104, 105, 103, 104, 0, 0, 10, 191, 1, 6, 4, 0, 35, 0, 11, 36, 1, 1, 126, 32, 0, 173, 32, 1, 173, 66, 32, 134, 132, 32, 2, 173, 32, 3, 173, 66, 32, 134, 132, 126, 34, 4, 66, 32, 135, 167, 36, 0, 32, 4, 167, 11, 36, 1, 1, 126, 32, 0, 173, 32, 1, 173, 66, 32, 134, 132, 32, 2, 173, 32, 3, 173, 66, 32, 134, 132, 127, 34, 4, 66, 32, 135, 167, 36, 0, 32, 4, 167, 11, 36, 1, 1, 126, 32, 0, 173, 32, 1, 173, 66, 32, 134, 132, 32, 2, 173, 32, 3, 173, 66, 32, 134, 132, 128, 34, 4, 66, 32, 135, 167, 36, 0, 32, 4, 167, 11, 36, 1, 1, 126, 32, 0, 173, 32, 1, 173, 66, 32, 134, 132, 32, 2, 173, 32, 3, 173, 66, 32, 134, 132, 129, 34, 4, 66, 32, 135, 167, 36, 0, 32, 4, 167, 11, 36, 1, 1, 126, 32, 0, 173, 32, 1, 173, 66, 32, 134, 132, 32, 2, 173, 32, 3, 173, 66, 32, 134, 132, 130, 34, 4, 66, 32, 135, 167, 36, 0, 32, 4, 167, 11
        ])), {}).exports;
    }
    catch (e) {
        // no wasm support :(
    }
    /** @internal */
    var INT_CACHE = {};
    /** @internal */
    var UINT_CACHE = {};
    /** @internal */
    var pow_dbl = Math.pow; // Used 4 times (4*8 to 15+4)
    /** @internal */
    var TWO_PWR_16_DBL = 1 << 16;
    /** @internal */
    var TWO_PWR_24_DBL = 1 << 24;
    /** @internal */
    var TWO_PWR_32_DBL = TWO_PWR_16_DBL * TWO_PWR_16_DBL;
    /** @internal */
    var TWO_PWR_64_DBL = TWO_PWR_32_DBL * TWO_PWR_32_DBL;
    /** @internal */
    var TWO_PWR_63_DBL = TWO_PWR_64_DBL / 2;
    /** @internal */
    var TWO_PWR_24 = fromInt(TWO_PWR_24_DBL);
    /** @internal */
    var ZERO = fromInt(0);
    /** @internal */
    var UZERO = fromInt(0, true);
    /** @internal */
    var ONE = fromInt(1);
    /** @internal */
    var UONE = fromInt(1, true);
    /** @internal */
    var NEG_ONE = fromInt(-1);
    /** @internal */
    var MAX_VALUE = fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0, false);
    /** @internal */
    var MAX_UNSIGNED_VALUE = fromBits(0xFFFFFFFF | 0, 0xFFFFFFFF | 0, true);
    /** @internal */
    var MIN_VALUE = fromBits(0, 0x80000000 | 0, false);
    function isLong(obj) {
        return (obj && obj["__isLong__"]) === true;
    }
    function fromInt(value, unsigned) {
        var obj, cachedObj, cache;
        if (unsigned) {
            value >>>= 0;
            if (cache = (0 <= value && value < 256)) {
                cachedObj = UINT_CACHE[value];
                if (cachedObj)
                    return cachedObj;
            }
            obj = fromBits(value, (value | 0) < 0 ? -1 : 0, true);
            if (cache)
                UINT_CACHE[value] = obj;
            return obj;
        }
        else {
            value |= 0;
            if (cache = (-128 <= value && value < 128)) {
                cachedObj = INT_CACHE[value];
                if (cachedObj)
                    return cachedObj;
            }
            obj = fromBits(value, value < 0 ? -1 : 0, false);
            if (cache)
                INT_CACHE[value] = obj;
            return obj;
        }
    }
    function fromNumber(value, unsigned) {
        if (isNaN(value))
            return unsigned ? UZERO : ZERO;
        if (unsigned) {
            if (value < 0)
                return UZERO;
            if (value >= TWO_PWR_64_DBL)
                return MAX_UNSIGNED_VALUE;
        }
        else {
            if (value <= -TWO_PWR_63_DBL)
                return MIN_VALUE;
            if (value + 1 >= TWO_PWR_63_DBL)
                return MAX_VALUE;
        }
        if (value < 0)
            return fromNumber(-value, unsigned).neg();
        return fromBits((value % TWO_PWR_32_DBL) | 0, (value / TWO_PWR_32_DBL) | 0, unsigned);
    }
    function fromBits(lowBits, highBits, unsigned) {
        return new Long(lowBits, highBits, unsigned);
    }
    function fromString(str, unsigned, radix) {
        if (str.length === 0)
            throw Error('empty string');
        if (str === "NaN" || str === "Infinity" || str === "+Infinity" || str === "-Infinity")
            return ZERO;
        if (typeof unsigned === 'number') {
            // For goog.math.long compatibility
            radix = unsigned,
                unsigned = false;
        }
        else {
            unsigned = !!unsigned;
        }
        radix = radix || 10;
        if (radix < 2 || 36 < radix)
            throw RangeError('radix');
        var p;
        if ((p = str.indexOf('-')) > 0)
            throw Error('interior hyphen');
        else if (p === 0) {
            return fromString(str.substring(1), unsigned, radix).neg();
        }
        // Do several (8) digits each time through the loop, so as to
        // minimize the calls to the very expensive emulated div.
        var radixToPower = fromNumber(pow_dbl(radix, 8));
        var result = ZERO;
        for (var i = 0; i < str.length; i += 8) {
            var size = Math.min(8, str.length - i), value = parseInt(str.substring(i, i + size), radix);
            if (size < 8) {
                var power = fromNumber(pow_dbl(radix, size));
                result = result.mul(power).add(fromNumber(value));
            }
            else {
                result = result.mul(radixToPower);
                result = result.add(fromNumber(value));
            }
        }
        result.unsigned = unsigned;
        return result;
    }
    function fromValue(val, unsigned) {
        if (typeof val === 'number')
            return fromNumber(val, unsigned);
        if (typeof val === 'string')
            return fromString(val, unsigned);
        // Throws for non-objects, converts non-instanceof Long:
        return fromBits(val.low, val.high, typeof unsigned === 'boolean' ? unsigned : val.unsigned);
    }
    function fromBytes(bytes, unsigned, le) {
        return le ? fromBytesLE(bytes, unsigned) : fromBytesBE(bytes, unsigned);
    }
    function fromBytesLE(bytes, unsigned) {
        return new Long(bytes[0] |
            bytes[1] << 8 |
            bytes[2] << 16 |
            bytes[3] << 24, bytes[4] |
            bytes[5] << 8 |
            bytes[6] << 16 |
            bytes[7] << 24, unsigned);
    }
    function fromBytesBE(bytes, unsigned) {
        return new Long(bytes[4] << 24 |
            bytes[5] << 16 |
            bytes[6] << 8 |
            bytes[7], bytes[0] << 24 |
            bytes[1] << 16 |
            bytes[2] << 8 |
            bytes[3], unsigned);
    }
    var Long = /** @class */ (function () {
        function Long(low, high, unsigned) {
            /** @internal */
            this.__isLong__ = true;
            this.eqz = this.isZero;
            this.eq = this.equals;
            this.neq = this.notEquals;
            this.ne = this.notEquals;
            this.lt = this.lessThan;
            this.lte = this.lessThanOrEqual;
            this.le = this.lessThanOrEqual;
            this.gt = this.greaterThan;
            this.gte = this.greaterThanOrEqual;
            this.ge = this.greaterThanOrEqual;
            this.comp = this.compare;
            this.neg = this.negate;
            this.sub = this.subtract;
            this.mul = this.multiply;
            this.div = this.divide;
            this.mod = this.modulo;
            this.rem = this.modulo;
            this.shl = this.shiftLeft;
            this.shr = this.shiftRight;
            this.shru = this.shiftRightUnsigned;
            this.shr_u = this.shiftRightUnsigned;
            this.rotl = this.rotateLeft;
            this.rotr = this.rotateRight;
            this.low = low | 0;
            this.high = high | 0;
            this.unsigned = !!unsigned;
        }
        Long.prototype.toInt = function () {
            return this.unsigned ? this.low >>> 0 : this.low;
        };
        Long.prototype.toNumber = function () {
            if (this.unsigned)
                return ((this.high >>> 0) * TWO_PWR_32_DBL) + (this.low >>> 0);
            return this.high * TWO_PWR_32_DBL + (this.low >>> 0);
        };
        Long.prototype.toString = function (radix) {
            radix = radix || 10;
            if (radix < 2 || 36 < radix)
                throw RangeError('radix');
            if (this.isZero())
                return '0';
            if (this.isNegative()) { // Unsigned Longs are never negative
                if (this.eq(MIN_VALUE)) {
                    // We need to change the Long value before it can be negated, so we remove
                    // the bottom-most digit in this base and then recurse to do the rest.
                    var radixLong = fromNumber(radix), div = this.div(radixLong), rem1 = div.mul(radixLong).sub(this);
                    return div.toString(radix) + rem1.toInt().toString(radix);
                }
                else
                    return '-' + this.neg().toString(radix);
            }
            // Do several (6) digits each time through the loop, so as to
            // minimize the calls to the very expensive emulated div.
            var radixToPower = fromNumber(pow_dbl(radix, 6), this.unsigned), rem = this;
            var result = '';
            while (true) {
                var remDiv = rem.div(radixToPower), intval = rem.sub(remDiv.mul(radixToPower)).toInt() >>> 0, digits = intval.toString(radix);
                rem = remDiv;
                if (rem.isZero())
                    return digits + result;
                else {
                    while (digits.length < 6)
                        digits = '0' + digits;
                    result = '' + digits + result;
                }
            }
        };
        Long.prototype.getHighBits = function () {
            return this.high;
        };
        Long.prototype.getHighBitsUnsigned = function () {
            return this.high >>> 0;
        };
        Long.prototype.getLowBits = function () {
            return this.low;
        };
        Long.prototype.getLowBitsUnsigned = function () {
            return this.low >>> 0;
        };
        Long.prototype.getNumBitsAbs = function () {
            if (this.isNegative()) // Unsigned Longs are never negative
                return this.eq(MIN_VALUE) ? 64 : this.neg().getNumBitsAbs();
            var val = this.high != 0 ? this.high : this.low;
            var bit;
            for (bit = 31; bit > 0; bit--)
                if ((val & (1 << bit)) != 0)
                    break;
            return this.high != 0 ? bit + 33 : bit + 1;
        };
        Long.prototype.isZero = function () {
            return this.high === 0 && this.low === 0;
        };
        Long.prototype.isNegative = function () {
            return !this.unsigned && this.high < 0;
        };
        Long.prototype.isPositive = function () {
            return this.unsigned || this.high >= 0;
        };
        Long.prototype.isOdd = function () {
            return (this.low & 1) === 1;
        };
        Long.prototype.isEven = function () {
            return (this.low & 1) === 0;
        };
        Long.prototype.equals = function (other) {
            if (!isLong(other))
                other = fromValue(other);
            if (this.unsigned !== other.unsigned && (this.high >>> 31) === 1 && (other.high >>> 31) === 1)
                return false;
            return this.high === other.high && this.low === other.low;
        };
        Long.prototype.notEquals = function (other) {
            return !this.eq(/* validates */ other);
        };
        Long.prototype.lessThan = function (other) {
            return this.comp(/* validates */ other) < 0;
        };
        Long.prototype.lessThanOrEqual = function (other) {
            return this.comp(/* validates */ other) <= 0;
        };
        Long.prototype.greaterThan = function (other) {
            return this.comp(/* validates */ other) > 0;
        };
        Long.prototype.greaterThanOrEqual = function (other) {
            return this.comp(/* validates */ other) >= 0;
        };
        Long.prototype.compare = function (other) {
            if (!isLong(other))
                other = fromValue(other);
            if (this.eq(other))
                return 0;
            var thisNeg = this.isNegative(), otherNeg = other.isNegative();
            if (thisNeg && !otherNeg)
                return -1;
            if (!thisNeg && otherNeg)
                return 1;
            // At this point the sign bits are the same
            if (!this.unsigned)
                return this.sub(other).isNegative() ? -1 : 1;
            // Both are positive if at least one is unsigned
            return (other.high >>> 0) > (this.high >>> 0) || (other.high === this.high && (other.low >>> 0) > (this.low >>> 0)) ? -1 : 1;
        };
        Long.prototype.negate = function () {
            if (!this.unsigned && this.eq(MIN_VALUE))
                return MIN_VALUE;
            return this.not().add(ONE);
        };
        Long.prototype.add = function (addend) {
            if (!isLong(addend))
                addend = fromValue(addend);
            // Divide each number into 4 chunks of 16 bits, and then sum the chunks.
            var a48 = this.high >>> 16;
            var a32 = this.high & 0xFFFF;
            var a16 = this.low >>> 16;
            var a00 = this.low & 0xFFFF;
            var b48 = addend.high >>> 16;
            var b32 = addend.high & 0xFFFF;
            var b16 = addend.low >>> 16;
            var b00 = addend.low & 0xFFFF;
            var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
            c00 += a00 + b00;
            c16 += c00 >>> 16;
            c00 &= 0xFFFF;
            c16 += a16 + b16;
            c32 += c16 >>> 16;
            c16 &= 0xFFFF;
            c32 += a32 + b32;
            c48 += c32 >>> 16;
            c32 &= 0xFFFF;
            c48 += a48 + b48;
            c48 &= 0xFFFF;
            return fromBits((c16 << 16) | c00, (c48 << 16) | c32, this.unsigned);
        };
        Long.prototype.subtract = function (subtrahend) {
            if (!isLong(subtrahend))
                subtrahend = fromValue(subtrahend);
            return this.add(subtrahend.neg());
        };
        Long.prototype.multiply = function (multiplier) {
            if (this.isZero())
                return ZERO;
            if (!isLong(multiplier))
                multiplier = fromValue(multiplier);
            // use wasm support if present
            if (wasm) {
                var low = wasm["mul"](this.low, this.high, multiplier.low, multiplier.high);
                return fromBits(low, wasm["get_high"](), this.unsigned);
            }
            if (multiplier.isZero())
                return ZERO;
            if (this.eq(MIN_VALUE))
                return multiplier.isOdd() ? MIN_VALUE : ZERO;
            if (multiplier.eq(MIN_VALUE))
                return this.isOdd() ? MIN_VALUE : ZERO;
            if (this.isNegative()) {
                if (multiplier.isNegative())
                    return this.neg().mul(multiplier.neg());
                else
                    return this.neg().mul(multiplier).neg();
            }
            else if (multiplier.isNegative())
                return this.mul(multiplier.neg()).neg();
            // If both longs are small, use float multiplication
            if (this.lt(TWO_PWR_24) && multiplier.lt(TWO_PWR_24))
                return fromNumber(this.toNumber() * multiplier.toNumber(), this.unsigned);
            // Divide each long into 4 chunks of 16 bits, and then add up 4x4 products.
            // We can skip products that would overflow.
            var a48 = this.high >>> 16;
            var a32 = this.high & 0xFFFF;
            var a16 = this.low >>> 16;
            var a00 = this.low & 0xFFFF;
            var b48 = multiplier.high >>> 16;
            var b32 = multiplier.high & 0xFFFF;
            var b16 = multiplier.low >>> 16;
            var b00 = multiplier.low & 0xFFFF;
            var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
            c00 += a00 * b00;
            c16 += c00 >>> 16;
            c00 &= 0xFFFF;
            c16 += a16 * b00;
            c32 += c16 >>> 16;
            c16 &= 0xFFFF;
            c16 += a00 * b16;
            c32 += c16 >>> 16;
            c16 &= 0xFFFF;
            c32 += a32 * b00;
            c48 += c32 >>> 16;
            c32 &= 0xFFFF;
            c32 += a16 * b16;
            c48 += c32 >>> 16;
            c32 &= 0xFFFF;
            c32 += a00 * b32;
            c48 += c32 >>> 16;
            c32 &= 0xFFFF;
            c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
            c48 &= 0xFFFF;
            return fromBits((c16 << 16) | c00, (c48 << 16) | c32, this.unsigned);
        };
        Long.prototype.divide = function (divisor) {
            if (!isLong(divisor))
                divisor = fromValue(divisor);
            if (divisor.isZero())
                throw Error('division by zero');
            // use wasm support if present
            if (wasm) {
                // guard against signed division overflow: the largest
                // negative number / -1 would be 1 larger than the largest
                // positive number, due to two's complement.
                if (!this.unsigned &&
                    this.high === -0x80000000 &&
                    divisor.low === -1 && divisor.high === -1) {
                    // be consistent with non-wasm code path
                    return this;
                }
                var low = (this.unsigned ? wasm["div_u"] : wasm["div_s"])(this.low, this.high, divisor.low, divisor.high);
                return fromBits(low, wasm["get_high"](), this.unsigned);
            }
            if (this.isZero())
                return this.unsigned ? UZERO : ZERO;
            var approx, rem, res;
            if (!this.unsigned) {
                // This section is only relevant for signed longs and is derived from the
                // closure library as a whole.
                if (this.eq(MIN_VALUE)) {
                    if (divisor.eq(ONE) || divisor.eq(NEG_ONE))
                        return MIN_VALUE; // recall that -MIN_VALUE == MIN_VALUE
                    else if (divisor.eq(MIN_VALUE))
                        return ONE;
                    else {
                        // At this point, we have |other| >= 2, so |this/other| < |MIN_VALUE|.
                        var halfThis = this.shr(1);
                        approx = halfThis.div(divisor).shl(1);
                        if (approx.eq(ZERO)) {
                            return divisor.isNegative() ? ONE : NEG_ONE;
                        }
                        else {
                            rem = this.sub(divisor.mul(approx));
                            res = approx.add(rem.div(divisor));
                            return res;
                        }
                    }
                }
                else if (divisor.eq(MIN_VALUE))
                    return this.unsigned ? UZERO : ZERO;
                if (this.isNegative()) {
                    if (divisor.isNegative())
                        return this.neg().div(divisor.neg());
                    return this.neg().div(divisor).neg();
                }
                else if (divisor.isNegative())
                    return this.div(divisor.neg()).neg();
                res = ZERO;
            }
            else {
                // The algorithm below has not been made for unsigned longs. It's therefore
                // required to take special care of the MSB prior to running it.
                if (!divisor.unsigned)
                    divisor = divisor.toUnsigned();
                if (divisor.gt(this))
                    return UZERO;
                if (divisor.gt(this.shru(1))) // 15 >>> 1 = 7 ; with divisor = 8 ; true
                    return UONE;
                res = UZERO;
            }
            // Repeat the following until the remainder is less than other:  find a
            // floating-point that approximates remainder / other *from below*, add this
            // into the result, and subtract it from the remainder.  It is critical that
            // the approximate value is less than or equal to the real value so that the
            // remainder never becomes negative.
            rem = this;
            while (rem.gte(divisor)) {
                // Approximate the result of division. This may be a little greater or
                // smaller than the actual value.
                approx = Math.max(1, Math.floor(rem.toNumber() / divisor.toNumber()));
                // We will tweak the approximate result by changing it in the 48-th digit or
                // the smallest non-fractional digit, whichever is larger.
                var log2 = Math.ceil(Math.log(approx) / Math.LN2), delta = (log2 <= 48) ? 1 : pow_dbl(2, log2 - 48), 
                // Decrease the approximation until it is smaller than the remainder.  Note
                // that if it is too large, the product overflows and is negative.
                approxRes = fromNumber(approx), approxRem = approxRes.mul(divisor);
                while (approxRem.isNegative() || approxRem.gt(rem)) {
                    approx -= delta;
                    approxRes = fromNumber(approx, this.unsigned);
                    approxRem = approxRes.mul(divisor);
                }
                // We know the answer can't be zero... and actually, zero would cause
                // infinite recursion since we would make no progress.
                if (approxRes.isZero())
                    approxRes = ONE;
                res = res.add(approxRes);
                rem = rem.sub(approxRem);
            }
            return res;
        };
        Long.prototype.modulo = function (divisor) {
            if (!isLong(divisor))
                divisor = fromValue(divisor);
            // use wasm support if present
            if (wasm) {
                var low = (this.unsigned ? wasm["rem_u"] : wasm["rem_s"])(this.low, this.high, divisor.low, divisor.high);
                return fromBits(low, wasm["get_high"](), this.unsigned);
            }
            return this.sub(this.div(divisor).mul(divisor));
        };
        Long.prototype.not = function () {
            return fromBits(~this.low, ~this.high, this.unsigned);
        };
        Long.prototype.and = function (other) {
            if (!isLong(other))
                other = fromValue(other);
            return fromBits(this.low & other.low, this.high & other.high, this.unsigned);
        };
        Long.prototype.or = function (other) {
            if (!isLong(other))
                other = fromValue(other);
            return fromBits(this.low | other.low, this.high | other.high, this.unsigned);
        };
        Long.prototype.xor = function (other) {
            if (!isLong(other))
                other = fromValue(other);
            return fromBits(this.low ^ other.low, this.high ^ other.high, this.unsigned);
        };
        Long.prototype.shiftLeft = function (numBits) {
            if (isLong(numBits))
                numBits = numBits.toInt();
            if ((numBits &= 63) === 0)
                return this;
            else if (numBits < 32)
                return fromBits((this.low << numBits), (this.high << numBits) | (this.low >>> (32 - numBits)), this.unsigned);
            else
                return fromBits(0, this.low << (numBits - 32), this.unsigned);
        };
        Long.prototype.shiftRight = function (numBits) {
            if (isLong(numBits))
                numBits = numBits.toInt();
            if ((numBits &= 63) === 0)
                return this;
            else if (numBits < 32)
                return fromBits((this.low >>> numBits) | (this.high << (32 - numBits)), this.high >> numBits, this.unsigned);
            else
                return fromBits(this.high >> (numBits - 32), this.high >= 0 ? 0 : -1, this.unsigned);
        };
        Long.prototype.shiftRightUnsigned = function (numBits) {
            if (isLong(numBits))
                numBits = numBits.toInt();
            if ((numBits &= 63) === 0)
                return this;
            if (numBits < 32)
                return fromBits((this.low >>> numBits) | (this.high << (32 - numBits)), this.high >>> numBits, this.unsigned);
            if (numBits === 32)
                return fromBits(this.high, 0, this.unsigned);
            return fromBits(this.high >>> (numBits - 32), 0, this.unsigned);
        };
        Long.prototype.rotateLeft = function (numBits) {
            var b;
            if (isLong(numBits))
                numBits = numBits.toInt();
            if ((numBits &= 63) === 0)
                return this;
            if (numBits === 32)
                return fromBits(this.high, this.low, this.unsigned);
            if (numBits < 32) {
                b = (32 - numBits);
                return fromBits(((this.low << numBits) | (this.high >>> b)), ((this.high << numBits) | (this.low >>> b)), this.unsigned);
            }
            numBits -= 32;
            b = (32 - numBits);
            return fromBits(((this.high << numBits) | (this.low >>> b)), ((this.low << numBits) | (this.high >>> b)), this.unsigned);
        };
        Long.prototype.rotateRight = function (numBits) {
            var b;
            if (isLong(numBits))
                numBits = numBits.toInt();
            if ((numBits &= 63) === 0)
                return this;
            if (numBits === 32)
                return fromBits(this.high, this.low, this.unsigned);
            if (numBits < 32) {
                b = (32 - numBits);
                return fromBits(((this.high << b) | (this.low >>> numBits)), ((this.low << b) | (this.high >>> numBits)), this.unsigned);
            }
            numBits -= 32;
            b = (32 - numBits);
            return fromBits(((this.low << b) | (this.high >>> numBits)), ((this.high << b) | (this.low >>> numBits)), this.unsigned);
        };
        Long.prototype.toSigned = function () {
            if (!this.unsigned)
                return this;
            return fromBits(this.low, this.high, false);
        };
        Long.prototype.toUnsigned = function () {
            if (this.unsigned)
                return this;
            return fromBits(this.low, this.high, true);
        };
        Long.prototype.toBytes = function (le) {
            return le ? this.toBytesLE() : this.toBytesBE();
        };
        Long.prototype.toBytesLE = function () {
            var hi = this.high, lo = this.low;
            return [
                lo & 0xff,
                lo >>> 8 & 0xff,
                lo >>> 16 & 0xff,
                lo >>> 24,
                hi & 0xff,
                hi >>> 8 & 0xff,
                hi >>> 16 & 0xff,
                hi >>> 24
            ];
        };
        Long.prototype.toBytesBE = function () {
            var hi = this.high, lo = this.low;
            return [
                hi >>> 24,
                hi >>> 16 & 0xff,
                hi >>> 8 & 0xff,
                hi & 0xff,
                lo >>> 24,
                lo >>> 16 & 0xff,
                lo >>> 8 & 0xff,
                lo & 0xff
            ];
        };
        Long.ZERO = ZERO;
        Long.UZERO = UZERO;
        Long.ONE = ONE;
        Long.UONE = UONE;
        Long.NEG_ONE = NEG_ONE;
        Long.MAX_VALUE = MAX_VALUE;
        Long.MAX_UNSIGNED_VALUE = MAX_UNSIGNED_VALUE;
        Long.MIN_VALUE = MIN_VALUE;
        Long.isLong = isLong;
        Long.fromInt = fromInt;
        Long.fromNumber = fromNumber;
        Long.fromBits = fromBits;
        Long.fromString = fromString;
        Long.fromValue = fromValue;
        Long.fromBytes = fromBytes;
        Long.fromBytesLE = fromBytesLE;
        Long.fromBytesBE = fromBytesBE;
        return Long;
    }());

    exports.Long = Long;

    Object.defineProperty(exports, '__esModule', { value: true });

})));
