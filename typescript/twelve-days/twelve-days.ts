const GIFTS: string[] = [
  'a Partridge in a Pear Tree', 'two Turtle Doves',
  'three French Hens',          'four Calling Birds',
  'five Gold Rings',            'six Geese-a-Laying',
  'seven Swans-a-Swimming',     'eight Maids-a-Milking',
  'nine Ladies Dancing',        'ten Lords-a-Leaping',
  'eleven Pipers Piping',       'twelve Drummers Drumming',
]

const ORDINALS: string[] = [
  'first', 'second', 'third', 'fourth', 'fifth', 'sixth',
  'seventh', 'eighth', 'ninth', 'tenth', 'eleventh', 'twelfth',
]

function verse(n: number): string {
  const gifts = GIFTS.slice(1, n).reverse().join(', ')
  const and = n > 1 ? ', and ' : ''

  return `On the ${ORDINALS[n - 1]} day of Christmas `
       + `my true love gave to me: ${gifts}${and}${GIFTS[0]}.\n`
}

export function recite(from: number, to: number): string {
  return Array.from({length: to - from + 1}, (_, i) => i + from)
    .map(verse)
    .join('')
}

