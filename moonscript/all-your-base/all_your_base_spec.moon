import rebase from require 'all_your_base'

describe 'all-your-base', ->
  it 'single bit one to decimal', ->
    result = rebase in_base: 2, out_base: 10, digits: {1}
    expected = {1}
    assert.are.same expected, result

  pending 'binary to single decimal', ->
    result = rebase in_base: 2, out_base: 10, digits: {1, 0, 1}
    expected = {5}
    assert.are.same expected, result

  pending 'single decimal to binary', ->
    result = rebase in_base: 10, out_base: 2, digits: {5}
    expected = {1, 0, 1}
    assert.are.same expected, result

  pending 'binary to multiple decimal', ->
    result = rebase in_base: 2, out_base: 10, digits: {1, 0, 1, 0, 1, 0}
    expected = {4, 2}
    assert.are.same expected, result

  pending 'decimal to binary', ->
    result = rebase in_base: 10, out_base: 2, digits: {4, 2}
    expected = {1, 0, 1, 0, 1, 0}
    assert.are.same expected, result

  pending 'trinary to hexadecimal', ->
    result = rebase in_base: 3, out_base: 16, digits: {1, 1, 2, 0}
    expected = {2, 10}
    assert.are.same expected, result

  pending 'hexadecimal to trinary', ->
    result = rebase in_base: 16, out_base: 3, digits: {2, 10}
    expected = {1, 1, 2, 0}
    assert.are.same expected, result

  pending '15-bit integer', ->
    result = rebase in_base: 97, out_base: 73, digits: {3, 46, 60}
    expected = {6, 10, 45}
    assert.are.same expected, result

  pending 'empty list', ->
    result = rebase in_base: 2, out_base: 10, digits: {}
    expected = {0}
    assert.are.same expected, result

  pending 'single zero', ->
    result = rebase in_base: 10, out_base: 2, digits: {0}
    expected = {0}
    assert.are.same expected, result

  pending 'multiple zeros', ->
    result = rebase in_base: 10, out_base: 2, digits: {0, 0, 0}
    expected = {0}
    assert.are.same expected, result

  pending 'leading zeros', ->
    result = rebase in_base: 7, out_base: 10, digits: {0, 6, 0}
    expected = {4, 2}
    assert.are.same expected, result

  pending 'input base is one', ->
    f = -> rebase in_base: 1, out_base: 10, digits: {0}
    assert.has.errors f, 'input base must be >= 2'

  pending 'input base is zero', ->
    f = -> rebase in_base: 0, out_base: 10, digits: {}
    assert.has.errors f, 'input base must be >= 2'

  pending 'input base is negative', ->
    f = -> rebase in_base: -2, out_base: 10, digits: {1}
    assert.has.errors f, 'input base must be >= 2'

  pending 'negative digit', ->
    f = -> rebase in_base: 2, out_base: 10, digits: {1, -1, 1, 0, 1, 0}
    assert.has.errors f, 'all digits must satisfy 0 <= d < input base'

  pending 'invalid positive digit', ->
    f = -> rebase in_base: 2, out_base: 10, digits: {1, 2, 1, 0, 1, 0}
    assert.has.errors f, 'all digits must satisfy 0 <= d < input base'

  pending 'output base is one', ->
    f = -> rebase in_base: 2, out_base: 1, digits: {1, 0, 1, 0, 1, 0}
    assert.has.errors f, 'output base must be >= 2'

  pending 'output base is zero', ->
    f = -> rebase in_base: 10, out_base: 0, digits: {7}
    assert.has.errors f, 'output base must be >= 2'

  pending 'output base is negative', ->
    f = -> rebase in_base: 2, out_base: -7, digits: {1}
    assert.has.errors f, 'output base must be >= 2'

  pending 'both bases are negative', ->
    f = -> rebase in_base: -2, out_base: -7, digits: {1}
    assert.has.errors f, 'input base must be >= 2'
