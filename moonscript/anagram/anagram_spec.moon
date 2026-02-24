find_anagrams = require 'anagram'

describe 'anagram', ->
  it 'no matches', ->
    result = find_anagrams 'diaper', {'hello', 'world', 'zombies', 'pants'}
    expected = {}
    assert.are.same expected, result

  it 'detects two anagrams', ->
    result = find_anagrams 'solemn', {'lemons', 'cherry', 'melons'}
    expected = {'lemons', 'melons'}
    assert.are.same expected, result

  it 'does not detect anagram subsets', ->
    result = find_anagrams 'good', {'dog', 'goody'}
    expected = {}
    assert.are.same expected, result

  it 'detects anagram', ->
    result = find_anagrams 'listen', {'enlists', 'google', 'inlets', 'banana'}
    expected = {'inlets'}
    assert.are.same expected, result

  it 'detects three anagrams', ->
    result = find_anagrams 'allergy', {'gallery', 'ballerina', 'regally', 'clergy', 'largely', 'leading'}
    expected = {'gallery', 'regally', 'largely'}
    assert.are.same expected, result

  it 'detects multiple anagrams with different case', ->
    result = find_anagrams 'nose', {'Eons', 'ONES'}
    expected = {'Eons', 'ONES'}
    assert.are.same expected, result

  it 'does not detect non-anagrams with identical checksum', ->
    result = find_anagrams 'mass', {'last'}
    expected = {}
    assert.are.same expected, result

  it 'detects anagrams case-insensitively', ->
    result = find_anagrams 'Orchestra', {'cashregister', 'Carthorse', 'radishes'}
    expected = {'Carthorse'}
    assert.are.same expected, result

  it 'detects anagrams using case-insensitive subject', ->
    result = find_anagrams 'Orchestra', {'cashregister', 'carthorse', 'radishes'}
    expected = {'carthorse'}
    assert.are.same expected, result

  it 'detects anagrams using case-insensitive possible matches', ->
    result = find_anagrams 'orchestra', {'cashregister', 'Carthorse', 'radishes'}
    expected = {'Carthorse'}
    assert.are.same expected, result

  it 'does not detect an anagram if the original word is repeated', ->
    result = find_anagrams 'go', {'goGoGO'}
    expected = {}
    assert.are.same expected, result

  it 'anagrams must use all letters exactly once', ->
    result = find_anagrams 'tapper', {'patter'}
    expected = {}
    assert.are.same expected, result

  it 'words are not anagrams of themselves', ->
    result = find_anagrams 'BANANA', {'BANANA'}
    expected = {}
    assert.are.same expected, result

  it 'words are not anagrams of themselves even if letter case is partially different', ->
    result = find_anagrams 'BANANA', {'Banana'}
    expected = {}
    assert.are.same expected, result

  it 'words are not anagrams of themselves even if letter case is completely different', ->
    result = find_anagrams 'BANANA', {'banana'}
    expected = {}
    assert.are.same expected, result

  it 'words other than themselves can be anagrams', ->
    result = find_anagrams 'LISTEN', {'LISTEN', 'Silent'}
    expected = {'Silent'}
    assert.are.same expected, result
