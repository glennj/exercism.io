/* eslint-disable no-return-assign */
export const isIsogram = (str) => {
  if (str === '') return true;
  const freq = {};
  const letter = new RegExp(/[a-z]/);
  str.toLowerCase()
    .split('')
    .filter(c => c.match(letter))
    .forEach(c => freq[c] = (freq[c] || 0) + 1);
  return Object.values(freq).every(val => val === 1);
};
