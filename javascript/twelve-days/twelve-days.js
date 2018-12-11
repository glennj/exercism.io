/* eslint-disable  class-methods-use-this */

const items = [
  'a Partridge in a Pear Tree',
  'two Turtle Doves',
  'three French Hens',
  'four Calling Birds',
  'five Gold Rings',
  'six Geese-a-Laying',
  'seven Swans-a-Swimming',
  'eight Maids-a-Milking',
  'nine Ladies Dancing',
  'ten Lords-a-Leaping',
  'eleven Pipers Piping',
  'twelve Drummers Drumming',
];

const nth = [
  'first', 'second', 'third', 'fourth', 'fifth', 'sixth',
  'seventh', 'eighth', 'ninth', 'tenth', 'eleventh', 'twelfth',
];

function* range(from, to, by = 1) {
  if (from < to) {
    for (let i = from; i <= to; i += by) yield i;
  } else {
    for (let i = from; i >= to; i -= by) yield i;
  }
}

class TwelveDays {
  verse(m, n = m) {
    return Array.from(range(m - 1, n - 1)).map((day) => {
      const gifts = Array.from(range(day, 0)).map(d => items[d]);
      if (gifts.length > 1) gifts.push(`and ${gifts.pop()}`);
      return `On the ${nth[day]} day of Christmas my true love gave to me: ${gifts.join(', ')}.\n`;
    }).join('\n');
  }

  sing() {
    return this.verse(1, items.length);
  }
}

module.exports = TwelveDays;

/* community
 *
 * generate a range of 0 to `to`: `range = [...Array(to+1).keys()];`
 *
 * good use of filter

      const days = ['', 'first', 'second', 'third', 'fourth', 'fifth',
          'sixth', 'seventh', 'eighth', 'ninth', 'tenth', 'eleventh',
          'twelfth'],
      gifts = ['', 'a Partridge in a Pear Tree.\n', 'two Turtle Doves, and ',
          'three French Hens, ', 'four Calling Birds, ', 'five Gold Rings, ',
          'six Geese-a-Laying, ', 'seven Swans-a-Swimming, ', 'eight Maids-a-Milking, ',
          'nine Ladies Dancing, ', 'ten Lords-a-Leaping, ', 'eleven Pipers Piping, ',
         'twelve Drummers Drumming, ']

      class TwelveDays {

        sing() {
          return this.verse(1, 12)
        }

        verse(from, to=from) {
          return [...Array(to+1).keys()].slice(from, to+1)
              .map(i => this.sentence(i))
              .join('\n')
        }

        sentence(no) {
          return 'On the ' +days[no] +' day of Christmas my true love gave to me, '
              +gifts.filter((_, i) => i <= no).reverse().join('')
        }
      }
 *
 */
