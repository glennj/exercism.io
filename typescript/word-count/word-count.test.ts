import { count } from './word-count'

describe('count()', () => {
  it('counts one word', () => {
    const expectedCounts = new Map(Object.entries({ word: 1 }))
    expect(count('word')).toEqual(expectedCounts)
  })

  it('counts one of each', () => {
    const expectedCounts = new Map(Object.entries({ one: 1, of: 1, each: 1 }))
    expect(count('one of each')).toEqual(expectedCounts)
  })

  it('counts multiple occurrences', () => {
    const expectedCounts = new Map(
      Object.entries({ one: 1, fish: 4, two: 1, red: 1, blue: 1 })
    )
    expect(count('one fish two fish red fish blue fish')).toEqual(
      expectedCounts
    )
  })

  it('handles cramped lists', () => {
    const expectedCounts = new Map(Object.entries({ one: 1, two: 1, three: 1 }))
    expect(count('one,two,three')).toEqual(expectedCounts)
  })

  it('handles expanded lists', () => {
    const expectedCounts = new Map(Object.entries({ one: 1, two: 1, three: 1 }))
    expect(count('one,\ntwo,\nthree')).toEqual(expectedCounts)
  })

  it('ignores punctuation', () => {
    const expectedCounts = new Map(
      Object.entries({
        car: 1,
        carpet: 1,
        as: 1,
        java: 1,
        javascript: 1,
      })
    )
    expect(count('car: carpet as java: javascript!!&@$%^&"')).toEqual(
      expectedCounts
    )
  })

  it('includes numbers', () => {
    const expectedCounts = new Map(Object.entries({ testing: 2, 1: 1, 2: 1 }))
    expect(count('testing, 1, 2 testing')).toEqual(expectedCounts)
  })

  it('normalizes case', () => {
    const expectedCounts = new Map(Object.entries({ go: 3, stop: 2 }))
    expect(count('go Go GO Stop stop')).toEqual(expectedCounts)
  })

  it('with apostrophes', () => {
    const expectedCounts = new Map(
      Object.entries({
        first: 1,
        "don't": 2,
        laugh: 1,
        then: 1,
        cry: 1,
        "you're": 1,
        getting: 1,
        it: 1,
      })
    )
    expect(
      count("'First: don't laugh. Then: don't cry. You're getting it.'")
    ).toEqual(expectedCounts)
  })

  it('substrings from the beginning', () => {
    const expectedCounts = new Map(
      Object.entries({
        joe: 1,
        "can't": 1,
        tell: 1,
        between: 1,
        app: 1,
        apple: 1,
        and: 1,
        a: 1,
      })
    )
    expect(count("Joe can't tell between app, apple and a.")).toEqual(
      expectedCounts
    )
  })

  it('multiple spaces not detected as a word', () => {
    const expectedCounts = new Map(
      Object.entries({ multiple: 1, whitespaces: 1 })
    )
    expect(count(' multiple   whitespaces')).toEqual(expectedCounts)
  })

  it('alternating word separators not detected as a word', () => {
    const expectedCounts = new Map(Object.entries({ one: 1, two: 1, three: 1 }))
    expect(count(",\n,one,\n ,two \n 'three'")).toEqual(expectedCounts)
  })

  it('quotation for word with apostrophe', () => {
    const expectedCounts = new Map(Object.entries({ can: 1, "can't": 2 }))
    expect(count("can, can't, 'can't'")).toEqual(expectedCounts)
  })

  it('handles properties that exist on Object’s prototype', () => {
    const expectedCounts = new Map(
      Object.entries({
        reserved: 1,
        words: 1,
        like: 1,
        constructor: 1,
        and: 1,
        tostring: 1,
        'ok?': 1,
      })
    )
    expect(count('reserved words like constructor and toString ok?')).toEqual(
      expectedCounts
    )
  })
})
