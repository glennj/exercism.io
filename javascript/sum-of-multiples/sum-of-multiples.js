/* eslint-disable  no-restricted-syntax */

const SumOfMultiples = nums => ({
  to: (limit) => {
    const multiples = new Set();
    nums.forEach((n) => {
      for (let i = 1; i * n < limit; i += 1) {
        multiples.add(i * n);
      }
    });
    let sum = 0;
    for (const m of multiples) sum += m;
    return sum;
  },
});

module.exports = SumOfMultiples;


/* community
 *
 * https://exercism.io/tracks/javascript/exercises/sum-of-multiples/solutions/83f265604a0e4b4fa7d45fbd186a6805
 * nifty range generator
 *

      export default (factors = [3, 5]) => {

        return Object.freeze({factors, to});

        function to(limit) {
          let multiples = {};
          factors
              .forEach(factor => {
                for (let ii of range(factor, limit, factor)) {
                  multiples[ii] = ii;
                }
              });
          return Object.keys(multiples)
              .reduce((prev, curr) => prev += multiples[curr], 0);
        }
      };

      function *range(curr, end, step) {
        if (curr >= end) return;
        yield curr;
        yield *range(curr += step, end, step);
      }

 *
 * tidier way to return
 *

      export default function SumOfMultiples(multiplies) {

        const
            MULT = multiplies || [3, 5],

            to = (n)  =>  {
              let res = 0 ;
              for (let i = 2; i < n; i += 1) {
                if ( MULT.some(m => i % m === 0) ) res += i;
              }
              return res;
            };

        return { to };
      }
 *
 */
