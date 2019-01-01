import PhoneNumber from './phone-number'

describe('PhoneNumber()', () => {

  it('cleans the number', () => {
    const phone = new PhoneNumber('(223) 456-7890')
    expect(phone.number()).toEqual('2234567890')
  })

  it('cleans numbers with dots', () => {
    const phone = new PhoneNumber('223.456.7890')
    expect(phone.number()).toEqual('2234567890')
  })

  it('cleans numbers with multiple spaces', () => {
    const phone = new PhoneNumber('223 456   7890   ')
    expect(phone.number()).toEqual('2234567890')
  })

  it('invalid when 9 digits', () => {
    const phone = new PhoneNumber('123456789')
    expect(phone.number()).toEqual(undefined)
  })

  it('invalid when 11 digits', () => {
    const phone = new PhoneNumber('21234567890')
    expect(phone.number()).toEqual(undefined)
  })

  it('valid when 11 digits and starting with 1', () => {
    const phone = new PhoneNumber('12234567890')
    expect(phone.number()).toEqual('2234567890')
  })

  describe('failures', () => {
    it('invalid with area code starting with 1', () => {
      const phone = new PhoneNumber('(123) 456-7890')
      expect(phone.number()).toEqual(undefined)
    })
    it('invalid with area code starting with 0', () => {
      const phone = new PhoneNumber('(023) 456-7890')
      expect(phone.number()).toEqual(undefined)
    })
    it('invalid with exchange starting with 1', () => {
      const phone = new PhoneNumber('(223) 156-7890')
      expect(phone.number()).toEqual(undefined)
    })
    it('invalid with exchange starting with 0', () => {
      const phone = new PhoneNumber('(223) 056-7890')
      expect(phone.number()).toEqual(undefined)
    })

    it('invalid when 12 digits', () => {
      const phone = new PhoneNumber('321234567890')
      expect(phone.number()).toEqual(undefined)
    })

    it('invalid with letters', () => {
      const phone = new PhoneNumber('123-abc-7890')
      expect(phone.number()).toEqual(undefined)
    })

    it('invalid with punctuations', () => {
      const phone = new PhoneNumber('123-@:!-7890')
      expect(phone.number()).toEqual(undefined)
    })

    it('invalid with right number of digits but letters mixed in', () => {
      const phone = new PhoneNumber('2a2b3c4d5e6f7g8h9i0j')
      expect(phone.number()).toEqual(undefined)
    })
  })

})
