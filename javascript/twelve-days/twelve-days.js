import {from} from './iterable-range';

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

export const recite = (m, n = m) => {
  return from(m - 1).upTo(n - 1).map((day) => {
    const gifts = from(day).downTo(0).map(d => items[d]);
    if (gifts.length > 1) gifts.push(`and ${gifts.pop()}`);
    return `On the ${nth[day]} day of Christmas my true love gave to me: ${gifts.join(', ')}.\n`;
  }).join('\n');
};
