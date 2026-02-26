PalindromeProducts = require 'palindrome_products'

describe 'palindrome-products', ->
  it 'find the smallest palindrome from single digit factors', ->
    palindrome, factors = PalindromeProducts.smallest 1, 9
    expected_palindrome = 1
    expected_factors    = {{1, 1}}
    assert.are.equal expected_palindrome, palindrome
    assert.are.same  expected_factors, factors

  it 'find the largest palindrome from single digit factors', ->
    palindrome, factors = PalindromeProducts.largest 1, 9
    expected_palindrome = 9
    expected_factors    = {{1, 9}, {3, 3}}
    assert.are.equal expected_palindrome, palindrome
    assert.are.same  expected_factors, factors

  it 'find the smallest palindrome from double digit factors', ->
    palindrome, factors = PalindromeProducts.smallest 10, 99
    expected_palindrome = 121
    expected_factors    = {{11, 11}}
    assert.are.equal expected_palindrome, palindrome
    assert.are.same  expected_factors, factors

  it 'find the largest palindrome from double digit factors', ->
    palindrome, factors = PalindromeProducts.largest 10, 99
    expected_palindrome = 9009
    expected_factors    = {{91, 99}}
    assert.are.equal expected_palindrome, palindrome
    assert.are.same  expected_factors, factors

  it 'find the smallest palindrome from triple digit factors', ->
    palindrome, factors = PalindromeProducts.smallest 100, 999
    expected_palindrome = 10201
    expected_factors    = {{101, 101}}
    assert.are.equal expected_palindrome, palindrome
    assert.are.same  expected_factors, factors

  it 'find the largest palindrome from triple digit factors', ->
    palindrome, factors = PalindromeProducts.largest 100, 999
    expected_palindrome = 906609
    expected_factors    = {{913, 993}}
    assert.are.equal expected_palindrome, palindrome
    assert.are.same  expected_factors, factors

  it 'find the smallest palindrome from four digit factors', ->
    palindrome, factors = PalindromeProducts.smallest 1000, 9999
    expected_palindrome = 1002001
    expected_factors    = {{1001, 1001}}
    assert.are.equal expected_palindrome, palindrome
    assert.are.same  expected_factors, factors

  it 'find the largest palindrome from four digit factors', ->
    palindrome, factors = PalindromeProducts.largest 1000, 9999
    expected_palindrome = 99000099
    expected_factors    = {{9901, 9999}}
    assert.are.equal expected_palindrome, palindrome
    assert.are.same  expected_factors, factors

  it 'empty result for smallest if no palindrome in the range', ->
    palindrome, factors = PalindromeProducts.smallest 1002, 1003
    expected_palindrome = nil
    expected_factors    = {}
    assert.are.equal expected_palindrome, palindrome
    assert.are.same  expected_factors, factors

  it 'empty result for largest if no palindrome in the range', ->
    palindrome, factors = PalindromeProducts.largest 15, 15
    expected_palindrome = nil
    expected_factors    = {}
    assert.are.equal expected_palindrome, palindrome
    assert.are.same  expected_factors, factors

  it 'error result for smallest if min is more than max', ->
    f = -> PalindromeProducts.smallest 10000, 1
    assert.has.errors f, 'min must be <= max'

  it 'error result for largest if min is more than max', ->
    f = -> PalindromeProducts.largest 2, 1
    assert.has.errors f, 'min must be <= max'

  it 'smallest product does not use the smallest factor', ->
    palindrome, factors = PalindromeProducts.smallest 3215, 4000
    expected_palindrome = 10988901
    expected_factors    = {{3297, 3333}}
    assert.are.equal expected_palindrome, palindrome
    assert.are.same  expected_factors, factors
