// -----------------------------------------------------------
const ALPHABET = 'abcdefghijklmnopqrstuvwxyz0123456789';
const M = 26;

// -----------------------------------------------------------
// Some math functions:
const gcd = (a, b) => (b == 0) ? a : gcd(b, a % b);

const mmi = function (a, m) {
  for (let n = 0; n < m; n++)
    if (a * n % m == 1)
      return n;
  throw new Error(`Cannot find MMI of ${a} and ${m}`);
};

// ensure the remainder is between [0, b), not negative
const floorMod = (a, b) => ((a % b) + b) % b;

// -----------------------------------------------------------
const validate = function(a) {
  if (gcd(a, M) != 1)
    throw new Error('a and m must be coprime.');
};

const xcode = (phrase, fn) => 
  phrase
    .toLowerCase()
    .split('')
    .map(c => ALPHABET.indexOf(c))
    .filter(i => i != -1)
    .map(i => i < M ? fn(i) : i)
    .map(j => ALPHABET[j])
    .join('');

// -----------------------------------------------------------
export const encode = (phrase, key) => {
  const {a, b} = key;
  validate(a);

  const coded = xcode(phrase, (x) => (a * x + b) % M);

  // add spaces
  return [...coded.matchAll(/.{1,5}/g)].join(" ");
};

export const decode = (phrase, key) => {
  const {a, b} = key;
  validate(a);
  const a_inv = mmi(a, M);

  const coded = xcode(phrase, (y) => floorMod(a_inv * (y - b), M));

  return coded;
};
