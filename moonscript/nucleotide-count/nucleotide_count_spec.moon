nucleotide_count = require 'nucleotide_count'

describe 'nucleotide-count', ->
  it 'empty strand', ->
    expected = A: 0, C: 0, G: 0, T: 0
    result = nucleotide_count ''
    assert.are.same expected, result

  pending 'can count one nucleotide in single-character input', ->
    expected = A: 0, C: 0, G: 1, T: 0
    result = nucleotide_count 'G'
    assert.are.same expected, result

  pending 'strand with repeated nucleotide', ->
    expected = A: 0, C: 0, G: 7, T: 0
    result = nucleotide_count 'GGGGGGG'
    assert.are.same expected, result

  pending 'strand with multiple nucleotides', ->
    expected = A: 20, C: 12, G: 17, T: 21
    result = nucleotide_count 'AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC'
    assert.are.same expected, result

  pending 'strand with invalid nucleotides', ->
    f = -> nucleotide_count 'AGXXACT'
    assert.has.errors f, 'Invalid nucleotide in strand'
