/* translation of this lovely recursive solution
 * https://exercism.io/tracks/javascript/exercises/say/solutions/515ab00bc90f46b0bde3732d9317a46b
 */

const SMALL: string[] = [
  'zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven',
  'eight', 'nine', 'ten', 'eleven', 'twelve', 'thirteen', 'fourteen',
  'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen'
]

const XTY: {[key: number]: string} = {
  20: 'twenty', 30: 'thirty',  40: 'forty',  50: 'fifty',
  60: 'sixty',  70: 'seventy', 80: 'eighty', 90: 'ninety'
}

const ARGUMENT_ERROR = new Error('Number must be between 0 and 999,999,999,999.')

class Say {
  static inEnglish (n: number): string {
    return new Say().say(n)
  }

  say(n: number): string {
    if (n < 0)    { throw ARGUMENT_ERROR }
    if (n < 100)  { return SMALL[n]
                        || XTY[n]
                        || `${this.say(n - n % 10)}-${this.say(n % 10)}` }
    if (n < 1000) { return this.sayCompound(n, 100, 'hundred') }
    if (n < 1e6)  { return this.sayCompound(n, 1e3, 'thousand') }
    if (n < 1e9)  { return this.sayCompound(n, 1e6, 'million') }
    if (n < 1e12) { return this.sayCompound(n, 1e9, 'billion') }
    throw ARGUMENT_ERROR
  }

  sayCompound (n: number, base: number, word: string): string {
    const rem = n % base
    return [
      this.say((n - rem) / base),
      word,
      rem && this.say(rem)
    ].filter((e) => e).join(' ')
  }
}

export function sayInEnglish(num: number): string {
  return Say.inEnglish(num)
}
