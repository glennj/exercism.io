/* eslint-disable  arrow-body-style, no-param-reassign, class-methods-use-this */

/* Change making algorithm from
 * http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf
 *
 * This function generates two arrays:
 *
 * C = maps the minimum number of coins required to make
 *     change for each n from 1 to amount.
 *
 * S = the _first_ coin used to make change for amount n
 *     (actually stores the coin _index_ into the
 *     denominations array)
 */

const change = (amount, denominations) => {
  const C = [0];
  const S = [];

  for (let n = 1; n <= amount; n += 1) {
    let min = Number.MAX_SAFE_INTEGER;
    let coin;

    for (let i = 0; i < denominations.length; i += 1) {
      if (denominations[i] <= n) {
        if (1 + C[n - denominations[i]] < min) {
          min = 1 + C[n - denominations[i]];
          coin = i;
        }
      }
    }
    C[n] = min;
    S[n] = coin;
  }

  return S;
};

const makeChange = (S, d, n) => {
  const result = [];

  if (S[n] === undefined) {
    throw new Error(`The total ${n} cannot be represented in the given currency.`);
  }

  while (n > 0) {
    const coin = d[S[n]];
    result.push(coin);
    n -= coin;
  }
  return result;
};

class Change {
  calculate(coins, amount) {
    if (amount < 0) throw new Error('Negative totals are not allowed.');
    if (amount === 0) return [];

    return makeChange(change(amount, coins), coins, amount);
  }
}

module.exports = Change;
