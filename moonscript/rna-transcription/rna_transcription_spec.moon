to_rna = require 'rna_transcription'

describe 'rna-transcription', ->
  it 'Empty RNA sequence', ->
    result = to_rna ''
    assert.are.equal '', result

  it 'RNA complement of cytosine is guanine', ->
    result = to_rna 'C'
    assert.are.equal 'G', result

  it 'RNA complement of guanine is cytosine', ->
    result = to_rna 'G'
    assert.are.equal 'C', result

  it 'RNA complement of thymine is adenine', ->
    result = to_rna 'T'
    assert.are.equal 'A', result

  it 'RNA complement of adenine is uracil', ->
    result = to_rna 'A'
    assert.are.equal 'U', result

  it 'RNA complement', ->
    result = to_rna 'ACGTGGTCTTAA'
    assert.are.equal 'UGCACCAGAAUU', result
