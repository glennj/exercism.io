/* eslint-disable  space-in-parens */

import {from} from './iterable-range';

// ASCII value of "@", character before "A"
const codeBase = 64;

export const rows = (ch) => {
  const n = ch.charCodeAt(0) - codeBase;

  // top half of the diamond
  const rows = from(1).upTo(n).map(i => {
    // left half of a row
    const a = new Array(n).fill(' ');
    a[i - 1] = String.fromCharCode(codeBase + i);
    // entire row
    return [...a].reverse().concat( a.slice(1) ).join('');
  });
  // append the bottom half of the diamond
  return rows.concat( [...rows].reverse().slice(1) );
};
