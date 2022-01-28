import {from} from './iterable-range'

export function reverse( text: string ): string {
  return from(text.length - 1)
    .downTo(0)
    .reduce((reversed, i) => reversed + text[i], '')
}
