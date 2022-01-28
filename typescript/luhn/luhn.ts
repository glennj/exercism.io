const LUHN_DIGIT = [
  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
  [0, 2, 4, 6, 8, 1, 3, 5, 7, 9],
]

const luhn = (input: string): boolean => {
  const noBlank = input.replace(/\s/g, '')
  if (noBlank.length <= 1 || /\D/.test(noBlank))
    return false

  const sum = [...noBlank]
    .map(Number)
    .reverse()
    .reduce((sum, d, i) => sum + LUHN_DIGIT[i%2][d])

  return sum % 10 === 0
}

export const valid = luhn
