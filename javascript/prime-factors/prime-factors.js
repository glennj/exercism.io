// using trial division algorithm
const primeFactors = (num) => {
  const factors = [];
  let n = num;
  while (n % 2 === 0) {
    factors.push(2);
    n /= 2;
  }
  let f = 3;
  while (f * f <= n) {
    if (n % f === 0) {
      factors.push(f);
      n /= f;
    } else {
      f += 2;
    }
  }
  if (n > 1) factors.push(n);
  return factors;
};

module.exports = { primeFactors };
