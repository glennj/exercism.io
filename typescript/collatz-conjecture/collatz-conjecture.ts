export function steps(n: number, step: number = 0): number {
  if (n <= 0)
    throw new Error('Only positive numbers are allowed')

  if (n === 1)
    return step

  n = (n % 2 === 0)
    ? n / 2
    : 3 * n + 1

  return steps(n, step + 1)
}
