/* eslint-disable no-return-assign */

/* We must pad row n to have at least as many characters as row n+1
 * So, for input ['111',   '22222', '3',   '44',  '555']
 * we should have padded string like
 *               ['111  ', '22222', '3  ', '44 ', '555']
 */
const padStrings = (a) => {
  const padded = [];
  a.reverse().forEach((s, i) => {
    if (i === 0) {
      padded.push(s);
    } else {
      const len = Math.max(s.length, padded[i - 1].length);
      padded.push(s.padEnd(len));
    }
  });
  return padded.reverse();
};

const transpose = (arrayOfStrings) => {
  const len = arrayOfStrings.length;
  if (len === 0) return [];
  const padded = padStrings(arrayOfStrings);
  const result = new Array(padded[0].length).fill('');
  padded.forEach((s) => {
    s.split('').forEach((c, i) => result[i] += c);
  });
  return result;
};

module.exports = transpose;
