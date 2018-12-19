/* eslint-disable no-restricted-syntax */
/* linter complains:
 * iterators/generators require regenerator-runtime, which is too
 * heavyweight for this guide to allow them. Separately, loops should be
 * avoided in favor of array iterations
 */

export const isPangram = (str) => {
  const lettersSeen = {};
  for (const ch of str.toLowerCase().split('')) {
    if (/[a-z]/.test(ch)) {
      lettersSeen[ch] = 1;
      if (Object.keys(lettersSeen).length === 26) {
        return true;
      }
    }
  }
  return false;
};

/* from the community: iterate over the alphabet, not the input string.

const alphabet = [...'abcdefghijklmnopqrstuvwxyz'];
export const isPangram = (str) => {
  const lowText = str.toLowerCase();
  return alphabet.every(letter => lowText.includes(letter));
};

*/
