const classify = (num: number): string => {
  if (num < 1 || num !== Math.floor(num)) {
    throw new Error('Classification is only possible for natural numbers.')
  }

  const factors: Set<number> = new Set()
  for (let i = Math.floor(Math.sqrt(num)); i > 0; i--) {
    if (num % i === 0) {
      factors.add(i).add(num / i)
    }
  }
  factors.delete(num)
  let sum = 0
  for (const factor of factors.values()) {
    sum = sum + factor
  }
  /* The following is much more compact but it will be much slower
   * since we have to iterate over ALL the numbers (twice),
   * not just the first sqrt(num) numbers

   const sum = new Array(num)
    .fill(0)
    .map((_, idx) => idx)
    .filter((i) => num % i === 0)
    .reduce((a, b) => a + b, 0)

   */

  if (sum < num) { return 'deficient' }
  if (sum > num) { return 'abundant' }
  return 'perfect'
}

export default { classify }
