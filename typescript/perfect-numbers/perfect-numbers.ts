export const classify = (num: number): string => {
  if (num < 1 || num !== Math.floor(num))
    throw new Error('Classification is only possible for natural numbers.')

  const factors: Set<number> = new Set()
  for (let i = Math.floor(Math.sqrt(num)); i > 0; i--)
    if (num % i === 0)
      factors.add(i).add(num / i)
  factors.delete(num)
  let sum = 0
  for (const factor of factors.values())
    sum += factor

  /* The following is more compact, but less efficient:
   * it has to iterate over the numbers from 0 to n-1 twice,
   * instead of just iterating from 0 to sqrt(n) once
   */
  /*
  const sum = Array.from({length: num}, (_, i) => i)
    .filter(i => num % i === 0)
    .reduce((sum, i) => sum + i, 0)
  */

  if (sum < num) return 'deficient'
  if (sum > num) return 'abundant'
  return 'perfect'
}
