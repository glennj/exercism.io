const tiles: Map<number, string[]> = new Map([
  [1,  ['A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T']],
  [2,  ['D', 'G']],
  [3,  ['B', 'C', 'M', 'P']],
  [4,  ['F', 'H', 'V', 'W', 'Y']],
  [5,  ['K']],
  [8,  ['J', 'X']],
  [10, ['Q', 'Z']],
])

function getValue(letter: string): number {
  for (const [value, letters] of tiles) {
    if (letters.includes(letter)) {
      return value
    }
  }
  throw new Error(`Unknown letter: ${letter}`)
}

export function score(letters: string = ''): number {
  return Array.from( letters.toUpperCase() )
    .reduce((score, letter) => score + getValue(letter), 0)
}
