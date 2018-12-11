const factors = (n) => {
  const f = [1];
  for (let i = Math.floor(Math.sqrt(n)); i > 1; i -= 1) {
    const j = n / i;
    if (j === Math.floor(j)) {
      f.push(i);
      if (i < j) f.push(j);
    }
  }
  return f;
};

export default class PerfectNumbers {
  //
  /* eslint-disable class-methods-use-this */
  classify(n) {
    if (n <= 0) throw new Error('Classification is only possible for natural numbers.');
    if (n === 1) return 'deficient';

    const s = factors(n).reduce((sum, num) => sum + num, 0);

    if (n === s) return 'perfect';
    if (n < s) return 'abundant';
    return 'deficient';
  }
}
