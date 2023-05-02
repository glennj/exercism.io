import { compute } from './hamming'

describe('Hamming', () => {
  it('empty strands', () => {
    expect(compute('', '')).toEqual(0)
  })

  it('single letter identical strands', () => {
    expect(compute('A', 'A')).toEqual(0)
  })

  it('single letter different strands', () => {
    expect(compute('G', 'T')).toEqual(1)
  })

  it('long identical strands', () => {
    expect(compute('GGACTGAAATCTG', 'GGACTGAAATCTG')).toEqual(0)
  })

  it('long different strands', () => {
    expect(compute('GGACGGATTCTG', 'AGGACGGATTCT')).toEqual(9)
  })

  it('disallow first strand longer', () => {
    expect(() => {
      compute('AATG', 'AAA')
    }).toThrowError('DNA strands must be of equal length.')
  })

  it('disallow second strand longer', () => {
    expect(() => {
      compute('ATA', 'AGTG')
    }).toThrowError('DNA strands must be of equal length.')
  })

  it('disallow empty first strand', () => {
    expect(() => {
      compute('', 'G')
    }).toThrowError('DNA strands must be of equal length.')
  })

  it('disallow empty second strand', () => {
    expect(() => {
      compute('G', '')
    }).toThrowError('DNA strands must be of equal length.')
  })
})
