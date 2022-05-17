class PhoneNumber {
  static clean(number: string): string {
    return new this(number).cleaned().number()
  }

  #number: string

  constructor(input: string = '') {
    this.#number = input
  }

  number(): string { return this.#number }

  cleaned(): this {
    return this
      .removeValidNondigits()
      .validateLength()
      .validateDigits()
      .trimCountryCode()
      .validIndex(0, 'Area code')
      .validIndex(3, 'Exchange code')
  }

  private removeValidNondigits(): this {
    this.#number = this.#number.replaceAll(/[(). +-]/g, '')
    return this
  }

  private validateLength(): this {
    if (this.#number.length < 10) throw new Error('Incorrect number of digits')
    if (this.#number.length > 11) throw new Error('More than 11 digits')
    if (this.#number.length === 11 && !this.#number.startsWith('1'))
      throw new Error('11 digits must start with 1')
    return this
  }

  private validateDigits(): this {
    // unicode char classes require the 'u' flag
    if (/\p{Letter}/u.test(this.#number)) throw new Error('Letters not permitted')
    if (/\D/.test(this.#number)) throw new Error('Punctuations not permitted')
    return this
  }

  private trimCountryCode(): this {
    if (this.#number.length === 11) this.#number = this.#number.substring(1)
    return this
  }

  private validIndex(i: number, what: string): this {
    const digit = this.#number[i]
    if (digit === '0') throw new Error(`${what} cannot start with zero`)
    if (digit === '1') throw new Error(`${what} cannot start with one`)
    return this
  }
}

export function clean(number: string): string {
  return PhoneNumber.clean(number)
}
