class Crypto {
  private normalized: string
  private rows: string[]
  private cols: string[]

  constructor(plaintext: string) {
    this.normalized = plaintext.replace(/\W/g, '').toLowerCase()
    const size = Math.ceil(Math.sqrt(this.normalized.length))
    this.rows = this.normalized.match(new RegExp(`.{1,${size}}`, 'g')) || []
    this.cols = [...new Array(size).keys()]
      .map((i) => this.rows.map((row) => row[i]).join(''))
  }

  normalizePlaintext(): string   { return this.normalized }
  size():               number   { return this.cols.length }
  plaintextSegments():  string[] { return this.rows }
  ciphertext():         string   { return this.cols.join('') }
}

export default Crypto
