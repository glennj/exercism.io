class Hamming {
  compute(a: string, b: string): number {
    if (a.length === b.length) {
      return [...a].filter((c, i) => c !== b[i]).length
    }
    throw new Error('DNA strands must be of equal length.')
  }
}

export default Hamming
