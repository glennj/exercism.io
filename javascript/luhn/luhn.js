const LUHN_DIGITS = [
  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
  [0, 2, 4, 6, 8, 1, 3, 5, 7, 9]
];
Object.freeze(LUHN_DIGITS);


export function valid(num) {
  if (/[^\d\s]/.test(num)) return false;

  const digits = num
    .replace(/\D/g, '')
    .split('')
    .reverse();

  if (digits.length <= 1) return false;

  const sum = digits
    .map(d => parseInt(d, 10))
    .reduce((s, d, i) => s + LUHN_DIGITS[i%2][d], 0);

  return (sum % 10 === 0);
}
