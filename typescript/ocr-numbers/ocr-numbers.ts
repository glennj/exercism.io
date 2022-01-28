// duck typing: a type T that can `length` and `slice` (Array and String)
interface CanSlice<T> {
  length: number
  slice: (a: number, b: number) => T
}

function* eachSlice<T>(obj: CanSlice<T>, size: number): IterableIterator<T> {
  for (let i = 0; i < obj.length; i += size) {
    yield obj.slice(i, i + size)
  }
}

const digitStrings = [
  ' _ | ||_|   ',
  '     |  |   ',
  ' _  _||_    ',
  ' _  _| _|   ',
  '   |_|  |   ',
  ' _ |_  _|   ',
  ' _ |_ |_|   ',
  ' _   |  |   ',
  ' _ |_||_|   ',
  ' _ |_| _|   ',
]

const digitLookup = (digitString: string): string => {
  const idx = digitStrings.indexOf(digitString)
  return (idx === -1 ? '?' : idx.toString())
}

const convertLine = (lines: string[]): string => {
  const slices = lines.map((line) => [...eachSlice(line, 3)])
  const digits = slices[0].map((_, i) => slices.map((slice) => slice[i]).join(''))
  return digits.map(digitLookup).join('')
}

export const convert = (text: string): string => {
  const lines = text.split('\n')
  if ( lines.length % 4 !== 0 || lines.some((l) => l.length % 3 !== 0) ) {
    throw new Error('Invalid input')
  }
  return [...eachSlice(lines, 4)].map(convertLine).join(',')
}

