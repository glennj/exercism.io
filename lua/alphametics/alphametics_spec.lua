local alphametics = require('alphametics')

describe('alphametics', function()

  it('should only accept simple arithmetic expressions', function()
    assert.has_error(function() alphametics.solve('string.lower("HELLO WORLD")') end)
  end)

  it('should solve short puzzles', function()
    local actual = alphametics.solve('I + BB == ILL')
    local expected = { I = 1, B = 9, L = 0 }
    assert.same(expected, actual)
  end)

  it('no leading zeroes', function()
    local actual = alphametics.solve('ACA + DD == BD')
    assert.falsy(actual)
  end)

  it('no solution', function()
    local actual = alphametics.solve('A == B')
    assert.falsy(actual)
  end)

  it('should solve puzzles with multiplication', function()
    local actual = alphametics.solve('IF * DR == DORI')
    local expected = { I = 8, F = 2, D = 3, R = 9, O = 1 }
    assert.same(expected, actual)
  end)

  it('should solve puzzles with any boolean expression', function()
    local actual = alphametics.solve('PI * R ^ 2 == AREA')
    local expected = { P = 9, I = 6, R = 7, A = 4, E = 0 }
    assert.same(expected, actual)
  end)

  it('test_puzzle_with_six_letters', function()
    local actual = alphametics.solve('NO + NO + TOO == LATE')
    local expected = { A = 0, E = 2, L = 1, N = 7, O = 4, T = 9 }
    assert.same(expected, actual)
  end)

  it('test_puzzle_with_seven_letters', function()
    local actual = alphametics.solve('HE + SEES + THE == LIGHT')
    local expected = { E = 4, G = 2, H = 5, I = 0, L = 1, S = 9, T = 7 }
    assert.same(expected, actual)
  end)

  it('should solve long puzzles', function()
    local actual = alphametics.solve('SEND + MORE == MONEY')
    local expected = { S = 9, E = 5, N = 6, D = 7, M = 1, O = 0, R = 8,Y = 2 }
    assert.same(expected, actual)
  end)

  it('there be pain here, 10 letters', function()
    local actual = alphametics.solve('AND + A + STRONG + OFFENSE + AS + A + GOOD == DEFENSE')
    local expected = { A = 5, D = 3, E = 4, F = 7, G = 8, N = 0, O = 2, R = 1, S = 6, T = 9 }
    assert.same(expected, actual)
  end)

end)
