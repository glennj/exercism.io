import proteins from require 'protein_translation'

describe 'protein-translation', ->
  it 'Empty RNA sequence results in no proteins', ->
    result = proteins ''
    expected = {}
    assert.are.same expected, result

  pending 'Methionine RNA sequence', ->
    result = proteins 'AUG'
    expected = {'Methionine'}
    assert.are.same expected, result

  pending 'Phenylalanine RNA sequence 1', ->
    result = proteins 'UUU'
    expected = {'Phenylalanine'}
    assert.are.same expected, result

  pending 'Phenylalanine RNA sequence 2', ->
    result = proteins 'UUC'
    expected = {'Phenylalanine'}
    assert.are.same expected, result

  pending 'Leucine RNA sequence 1', ->
    result = proteins 'UUA'
    expected = {'Leucine'}
    assert.are.same expected, result

  pending 'Leucine RNA sequence 2', ->
    result = proteins 'UUG'
    expected = {'Leucine'}
    assert.are.same expected, result

  pending 'Serine RNA sequence 1', ->
    result = proteins 'UCU'
    expected = {'Serine'}
    assert.are.same expected, result

  pending 'Serine RNA sequence 2', ->
    result = proteins 'UCC'
    expected = {'Serine'}
    assert.are.same expected, result

  pending 'Serine RNA sequence 3', ->
    result = proteins 'UCA'
    expected = {'Serine'}
    assert.are.same expected, result

  pending 'Serine RNA sequence 4', ->
    result = proteins 'UCG'
    expected = {'Serine'}
    assert.are.same expected, result

  pending 'Tyrosine RNA sequence 1', ->
    result = proteins 'UAU'
    expected = {'Tyrosine'}
    assert.are.same expected, result

  pending 'Tyrosine RNA sequence 2', ->
    result = proteins 'UAC'
    expected = {'Tyrosine'}
    assert.are.same expected, result

  pending 'Cysteine RNA sequence 1', ->
    result = proteins 'UGU'
    expected = {'Cysteine'}
    assert.are.same expected, result

  pending 'Cysteine RNA sequence 2', ->
    result = proteins 'UGC'
    expected = {'Cysteine'}
    assert.are.same expected, result

  pending 'Tryptophan RNA sequence', ->
    result = proteins 'UGG'
    expected = {'Tryptophan'}
    assert.are.same expected, result

  pending 'STOP codon RNA sequence 1', ->
    result = proteins 'UAA'
    expected = {}
    assert.are.same expected, result

  pending 'STOP codon RNA sequence 2', ->
    result = proteins 'UAG'
    expected = {}
    assert.are.same expected, result

  pending 'STOP codon RNA sequence 3', ->
    result = proteins 'UGA'
    expected = {}
    assert.are.same expected, result

  pending 'Sequence of two protein codons translates into proteins', ->
    result = proteins 'UUUUUU'
    expected = {'Phenylalanine', 'Phenylalanine'}
    assert.are.same expected, result

  pending 'Sequence of two different protein codons translates into proteins', ->
    result = proteins 'UUAUUG'
    expected = {'Leucine', 'Leucine'}
    assert.are.same expected, result

  pending 'Translate RNA strand into correct protein list', ->
    result = proteins 'AUGUUUUGG'
    expected = {'Methionine', 'Phenylalanine', 'Tryptophan'}
    assert.are.same expected, result

  pending 'Translation stops if STOP codon at beginning of sequence', ->
    result = proteins 'UAGUGG'
    expected = {}
    assert.are.same expected, result

  pending 'Translation stops if STOP codon at end of two-codon sequence', ->
    result = proteins 'UGGUAG'
    expected = {'Tryptophan'}
    assert.are.same expected, result

  pending 'Translation stops if STOP codon at end of three-codon sequence', ->
    result = proteins 'AUGUUUUAA'
    expected = {'Methionine', 'Phenylalanine'}
    assert.are.same expected, result

  pending 'Translation stops if STOP codon in middle of three-codon sequence', ->
    result = proteins 'UGGUAGUGG'
    expected = {'Tryptophan'}
    assert.are.same expected, result

  pending 'Translation stops if STOP codon in middle of six-codon sequence', ->
    result = proteins 'UGGUGUUAUUAAUGGUUU'
    expected = {'Tryptophan', 'Cysteine', 'Tyrosine'}
    assert.are.same expected, result

  pending 'Sequence of two non-STOP codons does not translate to a STOP codon', ->
    result = proteins 'AUGAUG'
    expected = {'Methionine', 'Methionine'}
    assert.are.same expected, result

  pending "Unknown amino acids, not part of a codon, can't translate", ->
    f = -> proteins 'XYZ'
    assert.has.errors f, 'Invalid codon'

  pending "Incomplete RNA sequence can't translate", ->
    f = -> proteins 'AUGU'
    assert.has.errors f, 'Invalid codon'

  pending 'Incomplete RNA sequence can translate if valid until a STOP codon', ->
    result = proteins 'UUCUUCUAAUGGU'
    expected = {'Phenylalanine', 'Phenylalanine'}
    assert.are.same expected, result
