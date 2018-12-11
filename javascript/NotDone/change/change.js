/* eslint-disable  arrow-body-style */

// ref https://en.wikipedia.org/wiki/Change-making_problem#Implementation
const getChangeMakingMatrix = (setOfCoins, r) => {
  const m = new Array(setOfCoins.length + 1).fill().map(() => new Array(r + 1).fill(0));
  m[0] = m[0].map((_, i) => i);
  return m;
};

const Change = () => {
  return {
    // this greedy algorithm fails for "non-standard" denominations.
    calculate: (values, sum) => {
      if (sum < 0) throw new Error('Negative totals are not allowed.');
      const minValue = Math.min(...values);
      const vals = values.sort((a, b) => b - a); // sort descending

      let tmp = sum;
      const coins = [];

      while (tmp > 0) {
        if (tmp < minValue) {
          throw new Error(`The total ${sum} cannot be represented in the given currency.`);
        }
        const coin = vals.find(c => c <= tmp); // eslint-disable-line  no-loop-func
        if (coin === undefined) {
          throw new Error(`Hmm. ${vals} ${sum} ${coins}`);
        }
        coins.push(coin);
        tmp -= coin;
      }
      return coins.sort((a, b) => a - b);
    },

    // ref https://en.wikipedia.org/wiki/Change-making_problem#Implementation
    smallestNumberOfCoins: (coins, n) => {
      const m = getChangeMakingMatrix(coins, n);
      for (let c = 1; c <= coins.length; c += 1) {
        for (let r = 1; r <= n; r += 1) {
          if (coins[c - 1] === r) {
            m[c][r] = 1;
          } else if (coins[c - 1] > r) {
            m[c][r] = m[c - 1][r];
          } else {
            m[c][r] = Math.min(m[c - 1][r], 1 + m[c][r - coins[c - 1]]);
          }
        }
      }
      return m[coins.length][n];
    },
  };
};

module.exports = Change;
