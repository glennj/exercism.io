/* eslint-disable  no-bitwise, no-multi-spaces */

const MSB = 1 << 7;       // 0b10000000 -> 1<<m === 2**m
const MASK = MSB ^ 0xff;  // 0b01111111
const SHIFT_AMT = 7;

/* Bit shifting wraps to -1 after (2**31)-1  (max 32 bit *signed* int)
 * From the ECMAScript standard (https://www.ecma-international.org/ecma-262/9.0/index.html#sec-left-shift-operator)
 * "... The result is a signed 32-bit integer."
 *
 * So n << 7 may overflow, and n >> 7 is wrong if n >= 2**31
 */
const shiftLeft = (n, m) => n * (2 ** m);               // n << m
const shiftRight = (n, m) => Math.floor(n / (2 ** m));  // n >> m

const encode = (numbers) => {
  let result = [];
  numbers.forEach((n) => {
    const bytes = [];
    let tmp = n;
    let msb = 0;
    do {
      bytes.unshift((tmp & MASK) | msb);
      msb = MSB;
      tmp = shiftRight(tmp, SHIFT_AMT);
    } while (tmp > 0);
    result = result.concat(bytes);
  });
  return result;
};

const decode = (bytes) => {
  if ((bytes[bytes.length - 1] & MSB) !== 0) throw new Error('Incomplete sequence');
  const { nums } = bytes.reduce((acc, b) => {
    acc.n = shiftLeft(acc.n, SHIFT_AMT) + (b & MASK);
    if ((b & MSB) === 0) {
      acc.nums.push(acc.n);
      acc.n = 0;
    }
    return acc;
  }, { nums: [], n: 0 });
  return nums;
};

module.exports = { encode, decode };
