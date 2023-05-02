import { clean } from './phone-number'

describe('Phone Number', () => {
  describe('Cleanup user-entered phone numbers', () => {
    it('cleans the number', () => {
      expect(clean('(223) 456-7890')).toEqual('2234567890')
    })

    it('cleans numbers with dots', () => {
      expect(clean('223.456.7890')).toEqual('2234567890')
    })

    it('cleans numbers with multiple spaces', () => {
      expect(clean('223 456   7890   ')).toEqual('2234567890')
    })

    it('invalid when 9 digits', () => {
      expect(() => clean('123456789')).toThrow(
        new Error('Incorrect number of digits')
      )
    })

    it('invalid when 11 digits does not start with a 1', () => {
      expect(() => clean('22234567890')).toThrow(
        new Error('11 digits must start with 1')
      )
    })

    it('valid when 11 digits and starting with 1', () => {
      expect(clean('12234567890')).toEqual('2234567890')
    })

    it('valid when 11 digits and starting with 1 even with punctuation', () => {
      expect(clean('+1 (223) 456-7890')).toEqual('2234567890')
    })

    it('invalid when more than 11 digits', () => {
      expect(() => clean('321234567890')).toThrow(
        new Error('More than 11 digits')
      )
    })

    it('invalid with letters', () => {
      expect(() => clean('123-abc-7890')).toThrow(
        new Error('Letters not permitted')
      )
    })

    it('invalid with punctuations', () => {
      expect(() => clean('123-@:!-7890')).toThrow(
        new Error('Punctuations not permitted')
      )
    })

    it('invalid if area code starts with 0', () => {
      expect(() => clean('(023) 456-7890')).toThrow(
        new Error('Area code cannot start with zero')
      )
    })

    it('invalid if area code starts with 1', () => {
      expect(() => clean('(123) 456-7890')).toThrow(
        new Error('Area code cannot start with one')
      )
    })

    it('invalid if exchange code starts with 0', () => {
      expect(() => clean('(223) 056-7890')).toThrow(
        new Error('Exchange code cannot start with zero')
      )
    })

    it('invalid if exchange code starts with 1', () => {
      expect(() => clean('(223) 156-7890')).toThrow(
        new Error('Exchange code cannot start with one')
      )
    })

    it('invalid if area code starts with 0 on valid 11-digit number', () => {
      expect(() => clean('1 (023) 456-7890')).toThrow(
        new Error('Area code cannot start with zero')
      )
    })

    it('invalid if area code starts with 1 on valid 11-digit number', () => {
      expect(() => clean('1 (123) 456-7890')).toThrow(
        new Error('Area code cannot start with one')
      )
    })

    it('invalid if exchange code starts with 0 on valid 11-digit number', () => {
      expect(() => clean('1 (223) 056-7890')).toThrow(
        new Error('Exchange code cannot start with zero')
      )
    })

    it('invalid if exchange code starts with 1 on valid 11-digit number', () => {
      expect(() => clean('1 (223) 156-7890')).toThrow(
        new Error('Exchange code cannot start with one')
      )
    })
  })
})
