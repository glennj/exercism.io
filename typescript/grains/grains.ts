const max = 64

/* The sensible way: */
function square(s: number): number {
  if (1 <= s && s <= max) {
    return Math.pow(2, s - 1)
  }
  return -1
}

function total(): number {
  return Math.pow(2, max) - 1
}

/* the plodding way: */
/*
const cache: number[] = []

function square(s: number): number {
  if (s < 1 || s > max) { return -1 }
  if (s === 1) { return 1 }
  if (cache[s] === undefined) {
    cache[s] = 2 * square(s - 1)
  }
  return cache[s]
}

function total(): number {
  let total = 0
  for (let i = 1; i <= max; i++) {
    total += square(i)
  }
  return total
}
*/

export default { square, total }