interface Count {
  [index: string]: number
  A: number
  C: number
  G: number
  T: number
}

export function nucleotideCounts(strand: string): Count {
  if (/[^ACGT]/.test(strand)) {
    throw new Error('Invalid nucleotide in strand')
  }
  const count: Count = {A: 0, C: 0, G: 0, T: 0}
  const segments = [...strand].sort().join('').match(/(.)\1*/g) || []
  segments.forEach((s) => count[s[0]] = s.length)
  return count
}
