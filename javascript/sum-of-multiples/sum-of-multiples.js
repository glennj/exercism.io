/* eslint-disable  no-restricted-syntax */

export const sum = (nums, limit) => {
  const multiples = new Set();
  nums
    .filter(n => n > 0)
    .forEach((n) => {
      for (let i = 1; i * n < limit; i++) {
        multiples.add(i * n);
      }
    });
  let sum = 0;
  for (const m of multiples) sum += m;
  return sum;
};
