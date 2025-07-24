/* eslint-disable no-multi-spaces */

export const clean = (str) => {
  // clean up legal non-digits
  let digits = str
    .replace(/^\+/, '')
    .replace(/\s/g, '')
    .replace(/[().-]/g, '')

  // note to self: must use `u` flag for the `\p{Category}`
  // character classes to match
  if (/\p{Letter}/u.test(digits))
    throw new Error('Letters not permitted');
  if (/\p{Punctuation}/u.test(digits))
    throw new Error('Punctuations not permitted');

  if (digits.length < 10)
    throw new Error('Must not be fewer than 10 digits');
  if (digits.length > 11)
    throw new Error('Must not be greater than 11 digits');
  if (digits.length === 11) {
    if (!digits.startsWith('1'))
      throw new Error('11 digits must start with 1');
    digits = digits.replace(/^1/, '');
  }

  if (digits[0] === '0')
    throw new Error('Area code cannot start with zero');
  if (digits[0] === '1')
    throw new Error('Area code cannot start with one');
  if (digits[3] === '0')
    throw new Error('Exchange code cannot start with zero');
  if (digits[3] === '1')
    throw new Error('Exchange code cannot start with one');

  return digits;
}
