export function compute(a: string, b: string): number {
  if (a.length !== b.length)
    throw new Error('DNA strands must be of equal length.')

  return [...a].filter((c, i) => c !== b[i]).length
}

