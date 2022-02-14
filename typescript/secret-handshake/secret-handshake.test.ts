import { commands } from './secret-handshake'

describe('Secret Handshake', () => {
  describe('Create A Handshake For A Number', () => {
    it('wink for 1', () => {
      expect(commands(1)).toEqual(['wink'])
    })

    it('double blink for 10', () => {
      expect(commands(2)).toEqual(['double blink'])
    })

    it('close your eyes for 100', () => {
      expect(commands(4)).toEqual(['close your eyes'])
    })

    it('jump for 1000', () => {
      expect(commands(8)).toEqual(['jump'])
    })

    it('combine two actions', () => {
      expect(commands(3)).toEqual(['wink', 'double blink'])
    })

    it('reverse two actions', () => {
      expect(commands(19)).toEqual(['double blink', 'wink'])
    })

    it('reversing one action gives the same action', () => {
      expect(commands(24)).toEqual(['jump'])
    })

    it('reversing no actions still gives no actions', () => {
      expect(commands(16)).toEqual([])
    })

    it('all possible actions', () => {
      expect(commands(15)).toEqual([
        'wink',
        'double blink',
        'close your eyes',
        'jump',
      ])
    })

    it('reverse all possible actions', () => {
      expect(commands(31)).toEqual([
        'jump',
        'close your eyes',
        'double blink',
        'wink',
      ])
    })

    it('do nothing for zero', () => {
      expect(commands(0)).toEqual([])
    })
  })
})
