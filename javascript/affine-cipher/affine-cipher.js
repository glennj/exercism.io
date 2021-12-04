// -----------------------------------------------------------
const CRYPTABLE_CHARS = 'abcdefghijklmnopqrstuvwxyz0123456789';
const M = 26;

// -----------------------------------------------------------
// Some math functions:
const gcd = (a, b) => (b == 0) ? a : gcd(b, a % b);

const mmi = (a, m) => {
  for (let n = 0; n < m; n++)
    if (a * n % m == 1)
      return n;
};

/* the remainder is between [0, b)
 *   -5 % 7 === -5
 *   floorMod(-5, 7) === 2
 */
const floorMod = (a, b) => ((a % b) + b) % b;

// -----------------------------------------------------------
const validateMultiplier = function(a) {
  if (gcd(a, M) != 1)
    throw new Error('a and m must be coprime.');
};

const encrypt = (phrase, fn) => 
  phrase
    .toLowerCase()
    .split('')
    .map(c => CRYPTABLE_CHARS.indexOf(c))
    .filter(i => i != -1)
    .map(i => i < M ? fn(i) : i)
    .map(j => CRYPTABLE_CHARS[j])
    .join('');

const grouped = (str) => [...str.matchAll(/.{1,5}/g)].join(" ");

// -----------------------------------------------------------
export const encode = (phrase, {a, b}) => {
  validateMultiplier(a);

  const coded = encrypt(phrase, (x) => floorMod(a * x + b, M));

  return grouped(coded);
};

export const decode = (phrase, {a, b}) => {
  validateMultiplier(a);
  const a_inv = mmi(a, M);

  const coded = encrypt(phrase, (y) => floorMod(a_inv * (y - b), M));

  return coded;
};
