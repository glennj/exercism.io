/* eslint-disable  no-extend-native, func-names */

// add "tr(1)" functionality to String
String.prototype.transliterate = function (fromChars, toChars) {
  if (fromChars.length === 0) return this;
  if (toChars.length === 0) throw new Error('"to" character set must be non-empty.');

  // Ensure the "to" set is at least the same length as the "from" set
  // by repeating the last char the requisite number of times.
  const toPadded = toChars.padEnd(fromChars.length, toChars[toChars.length - 1]);

  const trOne = (c) => {
    const idx = fromChars.indexOf(c);
    return idx > -1 ? toPadded[idx] : c;
  };
  return Array.from(this).map(trOne).join('');
};

/* the task: Atbash cipher */

const alphabet = 'abcdefghijklmnopqrstuvwxyz';
const reversed = Array.from(alphabet).reverse().join('');

const encode = (str) => {
  const filtered = str.toLowerCase().replace(/[\W_]/g, '');
  const encoded = filtered.transliterate(alphabet, reversed);
  return encoded.match(/.{1,5}/g).join(' ');
};

module.exports = { encode };
