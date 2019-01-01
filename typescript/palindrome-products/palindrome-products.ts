type Input   = { maxFactor: number, minFactor?: number }
type PalInfo = { value: number, factors: number[] }
type Output  = { smallest: PalInfo, largest: PalInfo}

function generate({maxFactor, minFactor = 1}: Input): Output {
  const factors = new Map<number, number[]>()
  for (let i = minFactor; i <= maxFactor; i++) {
    for (let j = i; j <= maxFactor; j++) {
      factors.set((i * j), [i, j])
    }
  }
  const pals = [...factors.keys()].filter(isPalindrome).sort((a, b) => a - b)
  const [small, large] = [pals[0], pals[pals.length - 1]]
  return {
    smallest: { value: small, factors: factors.get(small) || [] },
    largest:  { value: large, factors: factors.get(large) || [] },
  }
}

function isPalindrome(n: number): boolean {
  return String(n) === String(n).split('').reverse().join('')
}

export default generate
