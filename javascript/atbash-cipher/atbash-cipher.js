/* eslint-disable  no-extend-native, func-names */

// implement `tr(1)`
function tr(str, fromChars, toChars) {
  if (fromChars.length === 0) return str;
  if (toChars.length === 0) throw new Error('"to" character set must be non-empty.');

  // Ensure the "to" set is at least the same length as the "from" set
  // by repeating the last char the requisite number of times.
  const toPadded = toChars.padEnd(fromChars.length, toChars[toChars.length - 1]);

  const trOne = (c) => {
    const idx = fromChars.indexOf(c);
    return idx > -1 ? toPadded[idx] : c;
  };
  return Array.from(str).map(trOne).join('');
}

/* the task: Atbash cipher */

const alphabet = 'abcdefghijklmnopqrstuvwxyz';
const reversed = Array.from(alphabet).reverse().join('');

function decode(str) {
  const filtered = str.toLowerCase().replace(/[\W_]/g, '');
  return tr(filtered, alphabet, reversed);
}

function encode(str) {
  return decode(str).match(/.{1,5}/g).join(' ');
}

module.exports = { encode, decode };
