import { isValid } from './isbn-verifier'

describe('ISBN Verifier', () => {
  it('valid isbn', () => {
    expect(isValid('3-598-21508-8')).toBeTruthy()
  })

  it('invalid isbn check digit', () => {
    expect(isValid('3-598-21508-9')).toBeFalsy()
  })

  it('valid isbn with a check digit of 10', () => {
    expect(isValid('3-598-21507-X')).toBeTruthy()
  })

  it('check digit is a character other than X', () => {
    expect(isValid('3-598-21507-A')).toBeFalsy()
  })

  it('invalid check digit in isbn is not treated as zero', () => {
    expect(isValid('4-598-21507-B')).toBeFalsy()
  })

  it('invalid character in isbn is not treated as zero', () => {
    expect(isValid('3-598-2K507-0')).toBeFalsy()
  })

  it('X is only valid as a check digit', () => {
    expect(isValid('3-598-2X507-9')).toBeFalsy()
  })

  it('valid isbn without separating dashes', () => {
    expect(isValid('3598215088')).toBeTruthy()
  })

  it('isbn without separating dashes and X as check digit', () => {
    expect(isValid('359821507X')).toBeTruthy()
  })

  it('isbn without check digit and dashes', () => {
    expect(isValid('359821507')).toBeFalsy()
  })

  it('too long isbn', () => {
    expect(isValid('3-598-21507-XX')).toBeFalsy()
  })

  it('too long isbn and no dashes', () => {
    expect(isValid('3598215078X')).toBeFalsy()
  })

  it('too short isbn', () => {
    expect(isValid('00')).toBeFalsy()
  })

  it('isbn without check digit', () => {
    expect(isValid('3-598-21507')).toBeFalsy()
  })

  it('check digit of X should not be used for 0', () => {
    expect(isValid('3-598-21515-X')).toBeFalsy()
  })

  it('empty isbn', () => {
    expect(isValid('')).toBeFalsy()
  })

  it('input is 9 characters', () => {
    expect(isValid('134456729')).toBeFalsy()
  })

  it('invalid characters are not ignored after checking length', () => {
    expect(isValid('3132P34035')).toBeFalsy()
  })

  it('invalid characters are not ignored before checking length', () => {
    expect(isValid('3598P215088')).toBeFalsy()
  })

  it('input is too long but contains a valid isbn', () => {
    expect(isValid('98245726788')).toBeFalsy()
  })
})
