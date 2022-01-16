//

const ALPHABET = 'abcdefghijklmnopqrstuvwxyz'
const LEN = ALPHABET.length
const index = (char: string): number => ALPHABET.indexOf(char)

export class SimpleCipher {
  key: string = ''

  constructor(key?: string) {
    if (key !== undefined) {
      if (key.length === 0 || new RegExp(`[^${ALPHABET}]`).test(key)) {
        throw new Error('Bad key')
      }
      this.key = key
    }
    else {
      const randomInt = (max: number): number => Math.floor( max * Math.random() )
      for (let i = 0; i < 100; i++) {
        this.key += ALPHABET[randomInt(LEN)]
      }
    }
  }

  encode( plaintext: string ): string {
    return this.code(plaintext, (c, k) => (index(c) + index(k)) % LEN )
  }

  decode( ciphertext: string ): string {
    return this.code(ciphertext, (c, k) => (index(c) - index(k) + LEN) % LEN )
  }

  code( text: string, codeIdx: (c: string, k: string) => number): string {
    while (this.key.length < text.length) {
      this.key += this.key
    }
    let coded = ''
    for (let i = 0; i < text.length; i++) {
      coded += ALPHABET[ codeIdx( text[i], this.key[i] ) ]
    }
    return coded
  }
}
