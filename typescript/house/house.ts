type Actor = { who: string, what?: string }

const ACTORS: Actor[] = [
  { who: 'house that Jack built.' },
  { who: 'malt', what: 'lay in' },
  { who: 'rat', what: 'ate' },
  { who: 'cat', what: 'killed' },
  { who: 'dog', what: 'worried' },
  { who: 'cow with the crumpled horn', what: 'tossed' },
  { who: 'maiden all forlorn', what: 'milked' },
  { who: 'man all tattered and torn', what: 'kissed' },
  { who: 'priest all shaven and shorn', what: 'married' },
  { who: 'rooster that crowed in the morn', what: 'woke' },
  { who: 'farmer sowing his corn', what: 'kept' },
  { who: 'horse and the hound and the horn', what: 'belonged to' },
]

export function verse(n: number): string[] {
  const lines = [`This is the ${ACTORS[n - 1].who}`]
  for (let i = n - 1; i >= 1; i -= 1) {
    lines.push(`that ${ACTORS[i].what} the ${ACTORS[i - 1].who}`)
  }
  return lines
}

export function verses(from: number, to: number): string[] {
  const range = Array.from({length: to - from + 1}, (_, i) => i + from)
  const lyrics = range.reduce(
    (lines, n) => lines.concat('', verse(n)),
    new Array<string>()
  )
  return lyrics.slice(1)
}
