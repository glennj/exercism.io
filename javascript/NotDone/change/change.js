/* eslint-disable  arrow-body-style */

/* Change making algorithm from
 * http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf
 *
 * This function generates two arrays:
 *
 * C = maps the minimum number of coins required to make
 *     change for each n from 1 to amount.
 *     It is returned but only used internally in this
 *     application.
 *
 * S = the _first_ coin used to make change for amount n
 *     (actually stores the coin _index_ into the
 *     denominations array)
 */

const change = (amount, denominations) => {
  const C = [0];
  const S = [];

  for (let n = 1; n <= amount; n++) {
    let min = Number.MAX_SAFE_INTEGER;
    let coin;

    for (let i = 0; i < denominations.length; i++) {
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
  return [C, S];
}

const make_change = (S, d, n) => {
  const result = [];

  if (!(n in S)) {
    // can't render this amount with these coins
    return;
  }

  while (n > 0) {
    result.unshift(S[n]);
    n -= d[S[n]];
  }
  return result;
}

const Change = () => {
  return {
    calculate: (coins, amount) => {
      const [C, S] = change(amount, coins);
      return make_change(S, coins, amount);
    },
  };
};

module.exports = Change;
