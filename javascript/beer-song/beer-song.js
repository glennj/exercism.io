/* eslint-disable  no-multi-spaces */

import { from } from './iterable-range';

const bottle  = n => `${n || 'no more'} bottle${n !== 1 ? 's' : ''} of beer`;
const one     = n => (n > 1 ? 'one' : 'it');
const ucFirst = s => s.substring(0, 1).toUpperCase() + s.substring(1);

class Beer {
  static verse(n) {
    const first = `${ucFirst(bottle(n))} on the wall, ${bottle(n)}.`;
    let second;
    if (n === 0) {
      second = `Go to the store and buy some more, ${bottle(99)} on the wall.`;
    } else {
      second = `Take ${one(n)} down and pass it around, ${bottle(n - 1)} on the wall.`;
    }
    return [first, second, ""];
  }

  static sing(start = 99, stop = 0) {
    const stanzas = from(start)
                    .downTo(stop)
                    .flatMap(i => this.verse(i));
    // remove the trailing empty line
    stanzas.pop();
    return stanzas;
  }
}

export const recite = (start, num) => {
  return Beer.sing(start, start + 1 - num);
};
