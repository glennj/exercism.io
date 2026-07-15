nucleotide_count = require 'nucleotide_count'

describe 'nucleotide-count:', ->
  it 'empty strand', ->
    result = nucleotide_count ''
    expected = {A: 0, C: 0, G: 0, T: 0}
    assert.are.same expected, result

  it 'can count one nucleotide in single-character input', ->
    result = nucleotide_count 'G'
    expected = {A: 0, C: 0, G: 1, T: 0}
    assert.are.same expected, result

  it 'strand with repeated nucleotide', ->
    result = nucleotide_count 'GGGGGGG'
    expected = {A: 0, C: 0, G: 7, T: 0}
    assert.are.same expected, result

  it 'strand with multiple nucleotides', ->
    result = nucleotide_count 'AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC'
    expected = {A: 20, C: 12, G: 17, T: 21}
    assert.are.same expected, result

  it 'strand with invalid nucleotides', ->
    f = -> nucleotide_count 'AGXXACT'
    assert.has.errors f, 'Invalid nucleotide in strand'

