const Alphabet = new Set('abcdefghijklmnopqrstuvwxyz')

export function isPangram(input: string): boolean {
  const letters = input.toLowerCase().replace(/[^a-z]/g, '')

  //return new Set([...letters]).size === 26

  for (const letter of Alphabet)
    if (letters.indexOf(letter) === -1)
      return false
  return true
}

