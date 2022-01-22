/* A brute force solution.
 * Expect slow performance when the number of distinct letters grows.
 */

// return zero-padded numbers of length `size`, with no repeated digit
function* partialPermutations(size) {
  // start with "0123..."
  let n = parseInt(Array.from({ length: size }, (_, i) => i).join(''), 10);
  // stop at "9876..."
  const to = Number(Array.from({ length: size }, (_, i) => 9 - i).join(''));

  while (n <= to) {
    const s = n.toString().padStart(size, '0');
    if (! s.match(/(.).*\1/)) {
      yield s;
    }
    n += 1;
  }
}

// map a digit to each letter
const mapDigits = (letters, digitStr) => [...letters]
  .reduce((map, c, i) => {
    map[c] = Number(digitStr[i]);
    return map;
  }, Object.create(null));

// transliterate a word into a number
const digitize = (word, map) => [...word].reduce((a, b) => a.concat(map[b]), '');

export const solve = (equation, force = false) => {
  const words = equation.match(/[A-Z]+/g);
  const letters = [...new Set([...equation.replace(/[^A-Z]/g, '')]).values()].join('');
  if (letters.length > 10) {
    throw new Error('Cannot solve in base 10: too many letters');
  }
  if (letters.length > 7 && !force) {
    throw new Error(`This is too slow for solution with ${letters.length} letters`);
  }

  for (const digitStr of partialPermutations(letters.length)) {
    const map = mapDigits(letters, digitStr);
    const operands = words.map(word => digitize(word, map));

    // numbers not allowed to start with 0
    if (! operands.some(n => /^0/.test(n))) {
      const answer = Number(operands.pop());
      const sum = operands.map(Number).reduce((a, b) => a + b);
      if (sum === answer) {
        return map;
      }
    }
  }
  return null; // no solution found
};
