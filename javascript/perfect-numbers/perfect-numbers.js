  /* The following is more compact but it will be slower
   * since we have to iterate over ALL the numbers (twice),
   * not just the first sqrt(num) numbers

   const sum = new Array(num)
    .fill(0)
    .map((_, idx) => idx)
    .filter((i) => num % i === 0)
    .reduce((a, b) => a + b, 0)

   */

const aliquotSum = (n) => {
  // gather the factors
  const f = new Set();
  for (let i = Math.floor(Math.sqrt(n)); i >= 1; i -= 1) {
    const j = n / i;
    if (j === Math.floor(j)) {
      f.add(i).add(j);
    }
  }
  f.delete(n);

  let sum = 0;
  f.forEach(factor => sum += factor);
  return sum;
};

const classify = (n) => {
  if (n <= 0) throw new Error('Classification is only possible for natural numbers.');

  const s = aliquotSum(n);

  if (n < s) return 'abundant';
  if (n > s) return 'deficient';
  return 'perfect';
};

export { classify };
