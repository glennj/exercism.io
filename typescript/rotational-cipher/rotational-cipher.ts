const ALPHABET = 'abcdefghijklmnopqrstuvwxyz'

export function rotate(input: string, rotation: number = 13): string {
  return [...input].map((c) => rotateChar(c, rotation)).join('')
}

function rotateChar(c: string, n: number): string {
  const lc = c.toLowerCase()
  const i = ALPHABET.indexOf(lc)
  if (i === -1) { return c }
  const d = ALPHABET[ (i + n) % ALPHABET.length ]
  return c === lc ? d : d.toUpperCase()
}
