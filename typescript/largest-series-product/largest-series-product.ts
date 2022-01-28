export function largestProduct(input: string, span: number): number {
  if (/\D/.test(input))
    throw new Error('Digits input must only contain digits')

  const digits = input.split('').map(Number)
  if (span < 0)
    throw new Error('Span must be greater than zero')
  if (digits.length < span)
    throw new Error('Span must be smaller than string length')

  let maxProduct = -Infinity
  for (const s of slices(digits, span)) {
    const product = s.reduce((p, d) => p * d, 1)
    if (product > maxProduct) maxProduct = product
  }

  return maxProduct
}

function* slices(data: number[], len: number = 1): IterableIterator<number[]> {
  for (let i = 0; i <= data.length - len; i++)
    yield data.slice(i, i + len)
}
