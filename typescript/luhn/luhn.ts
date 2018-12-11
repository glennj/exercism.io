const double = (n: number): number => {
  const nn = n * 2
  return (nn > 9 ? nn - 9 : nn)
}

const luhn = (input: string): boolean => {
  const noBlank = input.replace(/\s/g, '')
  if (noBlank.length <= 1 || /\D/.test(noBlank)) {
    return false
  }
  const sum = [...noBlank].map(Number).reverse()
    .reduce((sum, d, i) => sum + (i % 2 ? double(d) : d), 0)
  return sum % 10 === 0
}

export default { valid: luhn }
