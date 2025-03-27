// @ts-check

/**
 * Convert an array of digits into a number.
 * Example: array2num([1,2,3,4]) => 1234
 */
const array2num = (ary) => Number(ary.join(""));

/**
 * Calculates the sum of the two input arrays.
 *
 * @param {number[]} array1
 * @param {number[]} array2
 * @returns {number} sum of the two arrays
 */
export function twoSum(array1, array2) {
  /*
  const sumArray = function(ary) {
    return ary.reduce((sum, num) => sum * 10 + num);
  }
  return sumArray(array1) + sumArray(array2);
  */
  // this is more to the spirit of the exercise
  return array2num(array1) + array2num(array2);
}

/**
 * Checks whether a number is a palindrome.
 *
 * @param {number} value
 * @returns {boolean}  whether the number is a palindrome or not
 */
export function luckyNumber(value) {
  const reverseNum = function(num) {
    let reversed = String(num).split("").reverse();
    return array2num(reversed);
    /* Again, not to the spirit of the exercise:
    let rev = 0;
    while (num > 0) {
      let digit = num % 10;
      rev = rev * 10 + digit;
      num = (num - digit) / 10;
    }
    return rev;
    */
  }
  return value == reverseNum(value);
}

/**
 * Determines the error message that should be shown to the user
 * for the given input value.
 *
 * @param {string|null|undefined} input
 * @returns {string} error message
 */
export function errorMessage(input) {
  // empty string, null, undefined
  if (!input) return "Required field";

  const num = Number(input);
  // 0 or NaN
  if (!num) return "Must be a number besides 0";

  return "";
}
