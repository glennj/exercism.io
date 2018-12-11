const alphabet = 'abcdefghijklmnopqrstuvwxyz';
const len = alphabet.length;
const index = c => alphabet.indexOf(c);

const encoded = (c, k) => alphabet[(index(c) + index(k)) % len];
const decoded = (c, k) => alphabet[(index(c) - index(k) + len) % len];

const generateRandomKey = () => {
  const getRandomInt = max => Math.floor(Math.random() * Math.floor(max));
  let key = '';
  for (let i = 1; i <= 100; i += 1) {
    key += alphabet[getRandomInt(len)];
  }
  return key;
};

export class Cipher {
  constructor(key) {
    if (typeof key === 'undefined') {
      this.key = generateRandomKey();
    } else if (/^[a-z]+$/.test(key)) {
      this.key = key;
    } else {
      throw new Error('Bad key');
    }
  }

  encode(str) {
    return this.xxcode(str, encoded);
  }

  decode(str) {
    return this.xxcode(str, decoded);
  }

  xxcode(str, f) {
    let coded = '';
    for (let i = 0; i < str.length; i += 1) {
      coded += f(str[i], this.keyCharAt(i));
    }
    return coded;
  }

  keyCharAt(idx) {
    return this.key.charAt(idx % this.key.length);
  }
}
