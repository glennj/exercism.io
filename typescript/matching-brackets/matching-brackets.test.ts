import { isPaired } from './matching-brackets'

describe('Matching Brackets', () => {
  it('paired square brackets', () => {
    expect(isPaired('[]')).toEqual(true)
  })

  it('empty string', () => {
    expect(isPaired('')).toEqual(true)
  })

  it('unpaired brackets', () => {
    expect(isPaired('[[')).toEqual(false)
  })

  it('wrong ordered brackets', () => {
    expect(isPaired('}{')).toEqual(false)
  })

  it('wrong closing bracket', () => {
    expect(isPaired('{]')).toEqual(false)
  })

  it('paired with whitespace', () => {
    expect(isPaired('{ }')).toEqual(true)
  })

  it('partially paired brackets', () => {
    expect(isPaired('{[])')).toEqual(false)
  })

  it('simple nested brackets', () => {
    expect(isPaired('{[]}')).toEqual(true)
  })

  it('several paired brackets', () => {
    expect(isPaired('{}[]')).toEqual(true)
  })

  it('paired and nested brackets', () => {
    expect(isPaired('([{}({}[])])')).toEqual(true)
  })

  it('unopened closing brackets', () => {
    expect(isPaired('{[)][]}')).toEqual(false)
  })

  it('unpaired and nested brackets', () => {
    expect(isPaired('([{])')).toEqual(false)
  })

  it('paired and wrong nested brackets', () => {
    expect(isPaired('[({]})')).toEqual(false)
  })

  it('paired and incomplete brackets', () => {
    expect(isPaired('{}[')).toEqual(false)
  })

  it('too many closing brackets', () => {
    expect(isPaired('[]]')).toEqual(false)
  })

  it('math expression', () => {
    expect(isPaired('(((185 + 223.85) * 15) - 543)/2')).toEqual(true)
  })

  it('complex latex expression', () => {
    expect(
      isPaired(
        '\\left(\\begin{array}{cc} \\frac{1}{3} & x\\\\ \\mathrm{e}^{x} &... x^2 \\end{array}\\right)'
      )
    ).toEqual(true)
  })
})
