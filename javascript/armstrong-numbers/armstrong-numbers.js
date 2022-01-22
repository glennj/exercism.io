/* eslint no-unused-vars: ["error", { "varsIgnorePattern": "^validate" }] */


const validateNumerically = (input) => {
  const num = parseInt(input, 10);
  const m = Math.ceil(Math.log10(num));
  let sum = 0;
  let n = num;
  while (n > 0) {
    sum += (n % 10) ** m;
    n = Math.floor(n / 10);
  }
  return sum === num;
};


const validateStringwise = (input) => {
  const digits = input.toString().trim().split('').map(d => parseInt(d, 10));
  const sum = digits.reduce((s, d) => s + (d ** digits.length), 0);
  return sum === parseInt(input, 10);
};


module.exports = { isArmstrongNumber: validateNumerically };
