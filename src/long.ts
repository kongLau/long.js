/** @internal */
interface WasmExports {
    mul(low: number, high: number, mulLow: number, mulHigh: number): number;

    get_high(): number;

    div_u(low: number, high: number, divLow: number, divHigh: number): number;

    div_s(low: number, high: number, divLow: number, divHigh: number): number;

    rem_u(low: number, high: number, divLow: number, divHigh: number): number;

    rem_s(low: number, high: number, divLow: number, divHigh: number): number;
}

let wasm: WasmExports = null;

try {
    wasm = new WebAssembly.Instance(
        new WebAssembly.Module(
            new Uint8Array([
                0, 97, 115, 109, 1, 0, 0, 0, 1, 13, 2, 96, 0, 1, 127, 96, 4, 127, 127, 127, 127, 1, 127, 3, 7, 6, 0, 1, 1, 1, 1, 1, 6, 6, 1, 127, 1, 65, 0, 11, 7, 50, 6, 3, 109, 117, 108, 0, 1, 5, 100, 105, 118, 95, 115, 0, 2, 5, 100, 105, 118, 95, 117, 0, 3, 5, 114, 101, 109, 95, 115, 0, 4, 5, 114, 101, 109, 95, 117, 0, 5, 8, 103, 101, 116, 95, 104, 105, 103, 104, 0, 0, 10, 191, 1, 6, 4, 0, 35, 0, 11, 36, 1, 1, 126, 32, 0, 173, 32, 1, 173, 66, 32, 134, 132, 32, 2, 173, 32, 3, 173, 66, 32, 134, 132, 126, 34, 4, 66, 32, 135, 167, 36, 0, 32, 4, 167, 11, 36, 1, 1, 126, 32, 0, 173, 32, 1, 173, 66, 32, 134, 132, 32, 2, 173, 32, 3, 173, 66, 32, 134, 132, 127, 34, 4, 66, 32, 135, 167, 36, 0, 32, 4, 167, 11, 36, 1, 1, 126, 32, 0, 173, 32, 1, 173, 66, 32, 134, 132, 32, 2, 173, 32, 3, 173, 66, 32, 134, 132, 128, 34, 4, 66, 32, 135, 167, 36, 0, 32, 4, 167, 11, 36, 1, 1, 126, 32, 0, 173, 32, 1, 173, 66, 32, 134, 132, 32, 2, 173, 32, 3, 173, 66, 32, 134, 132, 129, 34, 4, 66, 32, 135, 167, 36, 0, 32, 4, 167, 11, 36, 1, 1, 126, 32, 0, 173, 32, 1, 173, 66, 32, 134, 132, 32, 2, 173, 32, 3, 173, 66, 32, 134, 132, 130, 34, 4, 66, 32, 135, 167, 36, 0, 32, 4, 167, 11
            ])
        ),
        {}
    ).exports as unknown as WasmExports;
} catch (e) {
    // no wasm support :(
}

/** @internal */
const INT_CACHE = {}
/** @internal */
const UINT_CACHE = {}
/** @internal */
const pow_dbl = Math.pow; // Used 4 times (4*8 to 15+4)
/** @internal */
const TWO_PWR_16_DBL = 1 << 16;
/** @internal */
const TWO_PWR_24_DBL = 1 << 24;
/** @internal */
const TWO_PWR_32_DBL = TWO_PWR_16_DBL * TWO_PWR_16_DBL;
/** @internal */
const TWO_PWR_64_DBL = TWO_PWR_32_DBL * TWO_PWR_32_DBL;
/** @internal */
const TWO_PWR_63_DBL = TWO_PWR_64_DBL / 2;

/** @internal */
const TWO_PWR_24 = fromInt(TWO_PWR_24_DBL);
/** @internal */
const ZERO = fromInt(0);
/** @internal */
const UZERO = fromInt(0, true);
/** @internal */
const ONE = fromInt(1);
/** @internal */
const UONE = fromInt(1, true);
/** @internal */
const NEG_ONE = fromInt(-1);
/** @internal */
const MAX_VALUE = fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0, false);
/** @internal */
const MAX_UNSIGNED_VALUE = fromBits(0xFFFFFFFF | 0, 0xFFFFFFFF | 0, true);
/** @internal */
const MIN_VALUE = fromBits(0, 0x80000000 | 0, false);

function isLong(obj: any): obj is Long {
    return (obj && obj["__isLong__"]) === true;
}

function fromInt(value: number, unsigned?: boolean): Long {
    let obj, cachedObj, cache;
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
    } else {
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

function fromNumber(value: number, unsigned?: boolean): Long {
    if (isNaN(value))
        return unsigned ? UZERO : ZERO;
    if (unsigned) {
        if (value < 0)
            return UZERO;
        if (value >= TWO_PWR_64_DBL)
            return MAX_UNSIGNED_VALUE;
    } else {
        if (value <= -TWO_PWR_63_DBL)
            return MIN_VALUE;
        if (value + 1 >= TWO_PWR_63_DBL)
            return MAX_VALUE;
    }
    if (value < 0)
        return fromNumber(-value, unsigned).neg();
    return fromBits((value % TWO_PWR_32_DBL) | 0, (value / TWO_PWR_32_DBL) | 0, unsigned);
}

function fromBits(lowBits: number, highBits: number, unsigned?: boolean): Long {
    return new Long(lowBits, highBits, unsigned);
}

function fromString(str: string, unsigned?: boolean | number, radix?: number): Long {
    if (str.length === 0)
        throw Error('empty string');
    if (str === "NaN" || str === "Infinity" || str === "+Infinity" || str === "-Infinity")
        return ZERO;
    if (typeof unsigned === 'number') {
        // For goog.math.long compatibility
        radix = unsigned,
            unsigned = false;
    } else {
        unsigned = !!unsigned;
    }
    radix = radix || 10;
    if (radix < 2 || 36 < radix)
        throw RangeError('radix');

    let p;
    if ((p = str.indexOf('-')) > 0)
        throw Error('interior hyphen');
    else if (p === 0) {
        return fromString(str.substring(1), unsigned, radix).neg();
    }

    // Do several (8) digits each time through the loop, so as to
    // minimize the calls to the very expensive emulated div.
    let radixToPower = fromNumber(pow_dbl(radix, 8));

    let result = ZERO;
    for (let i = 0; i < str.length; i += 8) {
        let size = Math.min(8, str.length - i),
            value = parseInt(str.substring(i, i + size), radix);
        if (size < 8) {
            let power = fromNumber(pow_dbl(radix, size));
            result = result.mul(power).add(fromNumber(value));
        } else {
            result = result.mul(radixToPower);
            result = result.add(fromNumber(value));
        }
    }
    result.unsigned = unsigned;
    return result;
}

function fromValue(val: Long | number | string | { low: number, high: number, unsigned: boolean }, unsigned?: boolean): Long {
    if (typeof val === 'number')
        return fromNumber(val, unsigned);
    if (typeof val === 'string')
        return fromString(val, unsigned);
    // Throws for non-objects, converts non-instanceof Long:
    return fromBits(val.low, val.high, typeof unsigned === 'boolean' ? unsigned : val.unsigned);
}

function fromBytes(bytes: number[], unsigned?: boolean, le?: boolean): Long {
    return le ? fromBytesLE(bytes, unsigned) : fromBytesBE(bytes, unsigned);
}

function fromBytesLE(bytes: number[], unsigned?: boolean): Long {
    return new Long(
        bytes[0] |
        bytes[1] << 8 |
        bytes[2] << 16 |
        bytes[3] << 24,
        bytes[4] |
        bytes[5] << 8 |
        bytes[6] << 16 |
        bytes[7] << 24,
        unsigned
    );
}

function fromBytesBE(bytes: number[], unsigned?: boolean): Long {
    return new Long(
        bytes[4] << 24 |
        bytes[5] << 16 |
        bytes[6] << 8 |
        bytes[7],
        bytes[0] << 24 |
        bytes[1] << 16 |
        bytes[2] << 8 |
        bytes[3],
        unsigned
    );
}

export class Long {
    static ZERO = ZERO;
    static UZERO = UZERO;
    static ONE = ONE;
    static UONE = UONE;
    static NEG_ONE = NEG_ONE;
    static MAX_VALUE = MAX_VALUE;
    static MAX_UNSIGNED_VALUE = MAX_UNSIGNED_VALUE;
    static MIN_VALUE = MIN_VALUE;
    static isLong = isLong;
    static fromInt = fromInt;
    static fromNumber = fromNumber;
    static fromBits = fromBits;
    static fromString = fromString;
    static fromValue = fromValue;
    static fromBytes = fromBytes;
    static fromBytesLE = fromBytesLE;
    static fromBytesBE = fromBytesBE;

    low: number;
    high: number;
    unsigned: boolean;
    /** @internal */
    private __isLong__: boolean = true;

    eqz = this.isZero;
    eq = this.equals;
    neq = this.notEquals;
    ne = this.notEquals;
    lt = this.lessThan;
    lte = this.lessThanOrEqual;
    le = this.lessThanOrEqual;
    gt = this.greaterThan;
    gte = this.greaterThanOrEqual;
    ge = this.greaterThanOrEqual;
    comp = this.compare;
    neg = this.negate;
    sub = this.subtract;
    mul = this.multiply;
    div = this.divide;
    mod = this.modulo;
    rem = this.modulo;
    shl = this.shiftLeft;
    shr = this.shiftRight;
    shru = this.shiftRightUnsigned;
    shr_u = this.shiftRightUnsigned;
    rotl = this.rotateLeft;
    rotr = this.rotateRight;

    constructor(low: number, high: number, unsigned?: boolean) {
        this.low = low | 0;
        this.high = high | 0;
        this.unsigned = !!unsigned;
    }

    toInt(): number {
        return this.unsigned ? this.low >>> 0 : this.low;
    }

    toNumber(): number {
        if (this.unsigned)
            return ((this.high >>> 0) * TWO_PWR_32_DBL) + (this.low >>> 0);
        return this.high * TWO_PWR_32_DBL + (this.low >>> 0);
    }

    toString(radix?: number): string {
        radix = radix || 10;
        if (radix < 2 || 36 < radix)
            throw RangeError('radix');
        if (this.isZero())
            return '0';
        if (this.isNegative()) { // Unsigned Longs are never negative
            if (this.eq(MIN_VALUE)) {
                // We need to change the Long value before it can be negated, so we remove
                // the bottom-most digit in this base and then recurse to do the rest.
                let radixLong = fromNumber(radix),
                    div = this.div(radixLong),
                    rem1 = div.mul(radixLong).sub(this);
                return div.toString(radix) + rem1.toInt().toString(radix);
            } else
                return '-' + this.neg().toString(radix);
        }

        // Do several (6) digits each time through the loop, so as to
        // minimize the calls to the very expensive emulated div.
        let radixToPower = fromNumber(pow_dbl(radix, 6), this.unsigned),
            rem: Long = this;
        let result = '';
        while (true) {
            let remDiv = rem.div(radixToPower),
                intval = rem.sub(remDiv.mul(radixToPower)).toInt() >>> 0,
                digits = intval.toString(radix);
            rem = remDiv;
            if (rem.isZero())
                return digits + result;
            else {
                while (digits.length < 6)
                    digits = '0' + digits;
                result = '' + digits + result;
            }
        }
    }

    getHighBits(): number {
        return this.high;
    }

    getHighBitsUnsigned(): number {
        return this.high >>> 0;
    }

    getLowBits(): number {
        return this.low;
    }

    getLowBitsUnsigned(): number {
        return this.low >>> 0;
    }

    getNumBitsAbs(): number {
        if (this.isNegative()) // Unsigned Longs are never negative
            return this.eq(MIN_VALUE) ? 64 : this.neg().getNumBitsAbs();
        let val = this.high != 0 ? this.high : this.low;
        let bit;
        for (bit = 31; bit > 0; bit--)
            if ((val & (1 << bit)) != 0)
                break;
        return this.high != 0 ? bit + 33 : bit + 1;
    }

    isZero(): boolean {
        return this.high === 0 && this.low === 0;
    }

    isNegative(): boolean {
        return !this.unsigned && this.high < 0;
    }

    isPositive(): boolean {
        return this.unsigned || this.high >= 0;
    }

    isOdd(): boolean {
        return (this.low & 1) === 1;
    }

    isEven(): boolean {
        return (this.low & 1) === 0;
    }

    equals(other: Long | number | string): boolean {
        if (!isLong(other))
            other = fromValue(other);
        if (this.unsigned !== other.unsigned && (this.high >>> 31) === 1 && (other.high >>> 31) === 1)
            return false;
        return this.high === other.high && this.low === other.low;
    }

    notEquals(other: Long | number | string): boolean {
        return !this.eq(/* validates */ other);
    }

    lessThan(other: Long | number | string): boolean {
        return this.comp(/* validates */ other) < 0;
    }

    lessThanOrEqual(other: Long | number | string): boolean {
        return this.comp(/* validates */ other) <= 0;
    }

    greaterThan(other: Long | number | string): boolean {
        return this.comp(/* validates */ other) > 0;
    }

    greaterThanOrEqual(other: Long | number | string): boolean {
        return this.comp(/* validates */ other) >= 0;
    }

    compare(other: Long | number | string): number {
        if (!isLong(other))
            other = fromValue(other);
        if (this.eq(other))
            return 0;
        let thisNeg = this.isNegative(),
            otherNeg = other.isNegative();
        if (thisNeg && !otherNeg)
            return -1;
        if (!thisNeg && otherNeg)
            return 1;
        // At this point the sign bits are the same
        if (!this.unsigned)
            return this.sub(other).isNegative() ? -1 : 1;
        // Both are positive if at least one is unsigned
        return (other.high >>> 0) > (this.high >>> 0) || (other.high === this.high && (other.low >>> 0) > (this.low >>> 0)) ? -1 : 1;
    }

    negate(): Long {
        if (!this.unsigned && this.eq(MIN_VALUE))
            return MIN_VALUE;
        return this.not().add(ONE);
    }

    add(addend: Long | number | string): Long {
        if (!isLong(addend))
            addend = fromValue(addend);

        // Divide each number into 4 chunks of 16 bits, and then sum the chunks.

        let a48 = this.high >>> 16;
        let a32 = this.high & 0xFFFF;
        let a16 = this.low >>> 16;
        let a00 = this.low & 0xFFFF;

        let b48 = addend.high >>> 16;
        let b32 = addend.high & 0xFFFF;
        let b16 = addend.low >>> 16;
        let b00 = addend.low & 0xFFFF;

        let c48 = 0, c32 = 0, c16 = 0, c00 = 0;
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
    }

    subtract(subtrahend: Long | number | string): Long {
        if (!isLong(subtrahend))
            subtrahend = fromValue(subtrahend);
        return this.add(subtrahend.neg());
    }

    multiply(multiplier: Long | number | string): Long {
        if (this.isZero())
            return ZERO;
        if (!isLong(multiplier))
            multiplier = fromValue(multiplier);

        // use wasm support if present
        if (wasm) {
            let low = wasm["mul"](this.low,
                this.high,
                multiplier.low,
                multiplier.high);
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
        } else if (multiplier.isNegative())
            return this.mul(multiplier.neg()).neg();

        // If both longs are small, use float multiplication
        if (this.lt(TWO_PWR_24) && multiplier.lt(TWO_PWR_24))
            return fromNumber(this.toNumber() * multiplier.toNumber(), this.unsigned);

        // Divide each long into 4 chunks of 16 bits, and then add up 4x4 products.
        // We can skip products that would overflow.

        let a48 = this.high >>> 16;
        let a32 = this.high & 0xFFFF;
        let a16 = this.low >>> 16;
        let a00 = this.low & 0xFFFF;

        let b48 = multiplier.high >>> 16;
        let b32 = multiplier.high & 0xFFFF;
        let b16 = multiplier.low >>> 16;
        let b00 = multiplier.low & 0xFFFF;

        let c48 = 0, c32 = 0, c16 = 0, c00 = 0;
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
    }

    divide(divisor: Long | number | string): Long {
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
            let low = (this.unsigned ? wasm["div_u"] : wasm["div_s"])(
                this.low,
                this.high,
                divisor.low,
                divisor.high
            );
            return fromBits(low, wasm["get_high"](), this.unsigned);
        }

        if (this.isZero())
            return this.unsigned ? UZERO : ZERO;
        let approx, rem, res;
        if (!this.unsigned) {
            // This section is only relevant for signed longs and is derived from the
            // closure library as a whole.
            if (this.eq(MIN_VALUE)) {
                if (divisor.eq(ONE) || divisor.eq(NEG_ONE))
                    return MIN_VALUE;  // recall that -MIN_VALUE == MIN_VALUE
                else if (divisor.eq(MIN_VALUE))
                    return ONE;
                else {
                    // At this point, we have |other| >= 2, so |this/other| < |MIN_VALUE|.
                    let halfThis = this.shr(1);
                    approx = halfThis.div(divisor).shl(1);
                    if (approx.eq(ZERO)) {
                        return divisor.isNegative() ? ONE : NEG_ONE;
                    } else {
                        rem = this.sub(divisor.mul(approx));
                        res = approx.add(rem.div(divisor));
                        return res;
                    }
                }
            } else if (divisor.eq(MIN_VALUE))
                return this.unsigned ? UZERO : ZERO;
            if (this.isNegative()) {
                if (divisor.isNegative())
                    return this.neg().div(divisor.neg());
                return this.neg().div(divisor).neg();
            } else if (divisor.isNegative())
                return this.div(divisor.neg()).neg();
            res = ZERO;
        } else {
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
            let log2 = Math.ceil(Math.log(approx) / Math.LN2),
                delta = (log2 <= 48) ? 1 : pow_dbl(2, log2 - 48),

                // Decrease the approximation until it is smaller than the remainder.  Note
                // that if it is too large, the product overflows and is negative.
                approxRes = fromNumber(approx),
                approxRem = approxRes.mul(divisor);
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
    }

    modulo(divisor: Long | number | string): Long {
        if (!isLong(divisor))
            divisor = fromValue(divisor);

        // use wasm support if present
        if (wasm) {
            let low = (this.unsigned ? wasm["rem_u"] : wasm["rem_s"])(
                this.low,
                this.high,
                divisor.low,
                divisor.high
            );
            return fromBits(low, wasm["get_high"](), this.unsigned);
        }

        return this.sub(this.div(divisor).mul(divisor));
    }

    not(): Long {
        return fromBits(~this.low, ~this.high, this.unsigned);
    }

    and(other: Long | number | string): Long {
        if (!isLong(other))
            other = fromValue(other);
        return fromBits(this.low & other.low, this.high & other.high, this.unsigned);
    }

    or(other: Long | number | string): Long {
        if (!isLong(other))
            other = fromValue(other);
        return fromBits(this.low | other.low, this.high | other.high, this.unsigned);
    }

    xor(other: Long | number | string): Long {
        if (!isLong(other))
            other = fromValue(other);
        return fromBits(this.low ^ other.low, this.high ^ other.high, this.unsigned);
    }

    shiftLeft(numBits: Long | number): Long {
        if (isLong(numBits))
            numBits = numBits.toInt();
        if ((numBits &= 63) === 0)
            return this;
        else if (numBits < 32)
            return fromBits((this.low << numBits), (this.high << numBits) | (this.low >>> (32 - numBits)), this.unsigned);
        else
            return fromBits(0, this.low << (numBits - 32), this.unsigned);
    }

    shiftRight(numBits: Long | number): Long {
        if (isLong(numBits))
            numBits = numBits.toInt();
        if ((numBits &= 63) === 0)
            return this;
        else if (numBits < 32)
            return fromBits((this.low >>> numBits) | (this.high << (32 - numBits)), this.high >> numBits, this.unsigned);
        else
            return fromBits(this.high >> (numBits - 32), this.high >= 0 ? 0 : -1, this.unsigned);
    }

    shiftRightUnsigned(numBits: Long | number): Long {
        if (isLong(numBits)) numBits = numBits.toInt();
        if ((numBits &= 63) === 0) return this;
        if (numBits < 32) return fromBits((this.low >>> numBits) | (this.high << (32 - numBits)), this.high >>> numBits, this.unsigned);
        if (numBits === 32) return fromBits(this.high, 0, this.unsigned);
        return fromBits(this.high >>> (numBits - 32), 0, this.unsigned);
    }

    rotateLeft(numBits: Long | number): Long {
        let b;
        if (isLong(numBits)) numBits = numBits.toInt();
        if ((numBits &= 63) === 0) return this;
        if (numBits === 32) return fromBits(this.high, this.low, this.unsigned);
        if (numBits < 32) {
            b = (32 - numBits);
            return fromBits(((this.low << numBits) | (this.high >>> b)), ((this.high << numBits) | (this.low >>> b)), this.unsigned);
        }
        numBits -= 32;
        b = (32 - numBits);
        return fromBits(((this.high << numBits) | (this.low >>> b)), ((this.low << numBits) | (this.high >>> b)), this.unsigned);
    }

    rotateRight(numBits: Long | number): Long {
        let b;
        if (isLong(numBits)) numBits = numBits.toInt();
        if ((numBits &= 63) === 0) return this;
        if (numBits === 32) return fromBits(this.high, this.low, this.unsigned);
        if (numBits < 32) {
            b = (32 - numBits);
            return fromBits(((this.high << b) | (this.low >>> numBits)), ((this.low << b) | (this.high >>> numBits)), this.unsigned);
        }
        numBits -= 32;
        b = (32 - numBits);
        return fromBits(((this.low << b) | (this.high >>> numBits)), ((this.high << b) | (this.low >>> numBits)), this.unsigned);
    }

    toSigned(): Long {
        if (!this.unsigned)
            return this;
        return fromBits(this.low, this.high, false);
    }

    toUnsigned(): Long {
        if (this.unsigned)
            return this;
        return fromBits(this.low, this.high, true);
    }

    toBytes(le?: boolean): number[] {
        return le ? this.toBytesLE() : this.toBytesBE();
    }

    toBytesLE(): number[] {
        let hi = this.high,
            lo = this.low;
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
    }

    toBytesBE(): number[] {
        let hi = this.high,
            lo = this.low;
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
    }

}