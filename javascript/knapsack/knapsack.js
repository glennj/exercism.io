/*
 * Translating the pseudocode from https://en.wikipedia.org/wiki/Knapsack_problem
 *
 *  Assume w_{1}, w_{2}, ..., w_{n}, W are strictly positive integers. 
 *  Define m[i,w] to be the maximum value that can be attained with weight
 *  less than or equal to w using items up to i (first i items).
 * 
 *  We can define m[i,w] recursively as follows: 
 *  * m[0,w] = 0
 *  * m[i,w] = m[i-1,w] if w_{i} > w (the new item is more than the current weight limit)
 *  * m[i,w] = max(m[i-1,w], m[i-1,w-w_{i}]+v_{i}) if w_{i} <= w.
 *
 * The solution can then be found by calculating m[n,W]. 
 */

export const knapsack = (maximumWeight, items) => {
  const n = items.length;
  const m = [];
  m[0] = Array(maximumWeight+1).fill(0);

  for (let i = 1; i <= n; i++) {
    m[i] = Array(maximumWeight+1);

    let item = items[i-1];
    let wi = item.weight;
    let vi = item.value;

    for (let w = 0; w <= maximumWeight; w++) {
      m[i][w] = wi > w
                  ? m[i-1][w]
                  : Math.max(m[i-1][w], vi + m[i-1][w-wi]);
    }
  }

  return m[n][maximumWeight];
};