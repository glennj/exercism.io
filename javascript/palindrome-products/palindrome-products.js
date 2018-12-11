/* eslint key-spacing: ["error", { "align": "value" }] */

const isPalindrome = n => String(n) === String(n).split('').reverse().join('');

const generate = (opt) => {
  if (!opt || !opt.maxFactor) throw new Error('missing maxFactor option');

  const factors = {};
  for (let i = (opt.minFactor || 1); i <= opt.maxFactor; i += 1) {
    for (let j = i; j <= opt.maxFactor; j += 1) {
      factors[i * j] = [i, j];
    }
  }

  const pal = Object.keys(factors)
    .filter(isPalindrome)
    .map(Number)
    .sort((a, b) => a - b);

  const [small, large] = [pal[0], pal[pal.length - 1]];

  return {
    smallest: { value: small, factors: factors[small] },
    largest:  { value: large, factors: factors[large] },
  };
};

module.exports = generate;
