class AtbashCipher {
  private static A_Z: string = '0123456879_abcdefghijklmnopqrstuvwxyz'
  private static Z_A: string = '0123456879_zyxwvutsrqponmlkjihgfedcba'

  encode(str: string): string {
    return this.groupBy5( this.code(str) )
  }

  decode(str: string): string {
    return this.code(str)
  }

  code(str: string): string {
    return str
      .toLocaleLowerCase()
      .replace(/\W/g, '')
      .split('')
      .map(this.codeChar)
      .join('')
  }

  codeChar(c: string): string {
    return AtbashCipher.Z_A[ AtbashCipher.A_Z.indexOf(c) ]
  }

  groupBy5(str: string): string {
    return (str.match(/.{1,5}/g) || []).join(' ')
  }
}

export default AtbashCipher
