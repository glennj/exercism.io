type MaybePhoneNumber = string | undefined

class PhoneNumber {
  private _number: MaybePhoneNumber

  constructor(input: string = '') {
    if (/\d{3}.*\d{3}.*\d{4}/.test(input)) {
      const num = input.replace(/\D/g, '').replace(/^1/, '')
      if (/^(?:[^01]..){2}.{4}$/.test(num)) {
        this._number = num
      }
    }
  }

  number(): MaybePhoneNumber { return this._number }
}

export default PhoneNumber
