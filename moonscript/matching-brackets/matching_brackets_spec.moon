import is_paired from require 'matching_brackets'

describe 'matching-brackets', ->
  it 'paired square brackets', ->
    assert.is_true is_paired '[]'

  it 'empty string', ->
    assert.is_true is_paired ''

  it 'unpaired brackets', ->
    assert.is_false is_paired '[['

  it 'wrong ordered brackets', ->
    assert.is_false is_paired '}{'

  it 'wrong closing bracket', ->
    assert.is_false is_paired '{]'

  it 'paired with whitespace', ->
    assert.is_true is_paired '{ }'

  it 'partially paired brackets', ->
    assert.is_false is_paired '{[])'

  it 'simple nested brackets', ->
    assert.is_true is_paired '{[]}'

  it 'several paired brackets', ->
    assert.is_true is_paired '{}[]'

  it 'paired and nested brackets', ->
    assert.is_true is_paired '([{}({}[])])'

  it 'unopened closing brackets', ->
    assert.is_false is_paired '{[)][]}'

  it 'unpaired and nested brackets', ->
    assert.is_false is_paired '([{])'

  it 'paired and wrong nested brackets', ->
    assert.is_false is_paired '[({]})'

  it 'paired and wrong nested brackets but innermost are correct', ->
    assert.is_false is_paired '[({}])'

  it 'paired and incomplete brackets', ->
    assert.is_false is_paired '{}['

  it 'too many closing brackets', ->
    assert.is_false is_paired '[]]'

  it 'early unexpected brackets', ->
    assert.is_false is_paired ')()'

  it 'early mismatched brackets', ->
    assert.is_false is_paired '{)()'

  it 'math expression', ->
    assert.is_true is_paired '(((185 + 223.85) * 15) - 543)/2'

  it 'complex latex expression', ->
    assert.is_true is_paired '\\left(\\begin{array}{cc} \\frac{1}{3} & x\\\\ \\mathrm{e}^{x} &... x^2 \\end{array}\\right)'
