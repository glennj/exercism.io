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

/* community
 *
 * generator

    function * primeFactors(n){
      for(let factor = 2; n > 1; factor++){
        while(n % factor === 0){
          n /= factor
          yield factor
        }
      }
    }

    export default () => ({for: n => [...primeFactors(n)]})

 */

