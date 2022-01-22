export function isValid(str) {
  const chars = str.replace(/-/g, '').split('');
  const check = chars.pop();

  if (chars.length !== 9
      || !chars.every(c => /\d/.test(c))
      || !/[\dX]/.test(check)
  ) return false;

  const digits = chars.map(d => parseInt(d, 10));
  digits.push(check === 'X' ? 10 : parseInt(check, 10));
  const sum = digits.reduce((sum, d, i) => sum + d * (10 - i), 0)
  return sum % 11 === 0;
}
