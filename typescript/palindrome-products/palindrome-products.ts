type Input   = { maxFactor: number, minFactor: number }
type PalInfo = { value: number | null, factors: number[][] }


export function generate({maxFactor, minFactor}: Input): PalindromeProducts {
  if (minFactor > maxFactor)
    throw new Error('min must be <= max')
  return new PalindromeProducts(minFactor, maxFactor)
}


function divmod(num: number, div: number): [number, number] {
  return [Math.floor(num / div), num % div]
}

// ------------------------------------------------------------
class PalindromeProducts {
  private min: number
  private max: number
  private emptyResult: PalInfo = {value: null, factors: []}

  constructor(minFactor: number, maxFactor: number) {
    this.min = minFactor
    this.max = maxFactor
  }

  get smallest(): PalInfo {
    for (let p = this.min ** 2; p <= this.max ** 2; p++) {
      const result = this.isPalindromeProduct(p)
      if (result)
        return result
    }
    return this.emptyResult
  }

  get largest(): PalInfo {
    for (let p = this.max ** 2; p >= this.min ** 2; p--) {
      const result = this.isPalindromeProduct(p)
      if (result)
        return result
    }
    return this.emptyResult
  }

  private isPalindromeProduct(n: number): PalInfo | undefined {
    if (this.isPalindrome(n)) {
      const fs = this.factors(n)
      if (fs.length > 0)
        return {value: n, factors: fs}
    }
    return
  }

  private isPalindrome(n: number): boolean {
    return String(n) === String(n).split('').reverse().join('')
  }

  private factors(n: number): number[][] {
    const pairs = new Array<number[]>()
    const limit = Math.min(this.max, Math.sqrt(n))
    for (let i = this.min; i <= limit; i++) {
      const [j, rem] = divmod(n, i)
      if (rem === 0 && this.min <= j && j <= this.max)
        pairs.push([i, j])
    }
    return pairs
  }
}
