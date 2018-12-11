const steps = (n) => {
  if (n <= 0) throw new Error('Only positive numbers are allowed');
  if (n === 1) return 0;
  return 1 + steps((n % 2) ? 3 * n + 1 : n / 2);
};

module.exports = { steps };
