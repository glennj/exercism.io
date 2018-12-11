const solve = (a, b) => {
  const x = parseFloat(a);
  const y = parseFloat(b);
  if (Number.isNaN(x) || Number.isNaN(y)) return null;

  const dist = Math.sqrt((x ** 2) + (y ** 2));
  if (dist <= 1) return 10;
  if (dist <= 5) return 5;
  if (dist <= 10) return 1;
  return 0;
};

module.exports = solve;
