class Converter {
  convert(digits: number[], from: number, to: number): number[] {
    // validation
    if (from <= 1 || !Number.isInteger(from)) {
      throw new Error('Wrong input base')
    }
    if (to <= 1 || !Number.isInteger(to)) {
      throw new Error('Wrong output base')
    }
    if ( digits.length === 0
      || (digits.length > 1 && digits[0] === 0)
      || digits.some((d) => d < 0 || d >= from || !Number.isInteger(d))
    ) {
      throw new Error('Input has wrong format')
    }

    // conversion
    let decimal = digits.reduce((s, d) => s * from + d, 0)
    const toDigits = []
    do {
      toDigits.unshift(decimal % to)
      decimal = Math.floor(decimal / to)
    } while (decimal > 0)
    return toDigits
  }
}

export default Converter
