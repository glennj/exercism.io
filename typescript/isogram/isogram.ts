export function isIsogram( input: string ): boolean {
  const letters = [...input.toLowerCase()].filter(c => /\p{Letter}/u.test(c))
  const seen = new Set()
  for (const letter of letters) {
    if (seen.has(letter)) return false
    seen.add(letter)
  }
  return true
}
