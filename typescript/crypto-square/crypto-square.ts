export class Crypto {
  private plaintxt: string
  private ciphertxt!: string

  constructor(plaintext: string) {
    this.plaintxt = plaintext
  }

  get ciphertext(): string {
    if (this.ciphertxt === undefined) {
      const normalized = this.plaintxt
        .replace(/\W/g, '')
        .toLowerCase()

      if (normalized.length === 0)
        return ''

      const size = Math.ceil(Math.sqrt(normalized.length))
      const rows = normalized
        .match(new RegExp(`.{1,${size}}`, 'g'))
        ?.map(row => row.padEnd(size))
        ?? []

      const cols = [...new Array(size).keys()]
        .map((i) => rows.map((row) => row[i]).join(''))

      this.ciphertxt = cols.join(' ')
    }
    return this.ciphertxt
  }
}
