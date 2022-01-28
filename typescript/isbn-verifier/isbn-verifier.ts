export function isValid(isbn: string): boolean {
  const chars: Array<string> = [...isbn].filter(c => /[\dX]/.test(c))

  if (chars.length !== 10) return false
  if (chars.some((c, i) => i < 9 && c === 'X')) return false

  const check = chars.pop()
  const digits = chars.map(Number)
  digits.push(check === 'X' ? 10 : Number(check))
  const sum = digits.reduce((sum, d, i) => sum + d * (10 - i), 0)
  return sum % 11 === 0
}
