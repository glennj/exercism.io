import transpose from require 'transpose'

describe 'transpose', ->
  it 'empty string', ->
    input = {}
    expected = {}
    assert.are.same expected, transpose input

  it 'two characters in a row', ->
    input = {'A1'}
    expected = {'A', '1'}
    assert.are.same expected, transpose input

  it 'two characters in a column', ->
    input = {'A', '1'}
    expected = {'A1'}
    assert.are.same expected, transpose input

  it 'simple', ->
    input = {'ABC', '123'}
    expected = {
      'A1',
      'B2',
      'C3',
    }
    assert.are.same expected, transpose input

  it 'single line', ->
    input = {'Single line.'}
    expected = {
      'S',
      'i',
      'n',
      'g',
      'l',
      'e',
      ' ',
      'l',
      'i',
      'n',
      'e',
      '.',
    }
    assert.are.same expected, transpose input

  it 'first line longer than second line', ->
    input = {'The fourth line.', 'The fifth line.'}
    expected = {
      'TT',
      'hh',
      'ee',
      '  ',
      'ff',
      'oi',
      'uf',
      'rt',
      'th',
      'h ',
      ' l',
      'li',
      'in',
      'ne',
      'e.',
      '.',
    }
    assert.are.same expected, transpose input

  it 'second line longer than first line', ->
    input = {'The first line.', 'The second line.'}
    expected = {
      'TT',
      'hh',
      'ee',
      '  ',
      'fs',
      'ie',
      'rc',
      'so',
      'tn',
      ' d',
      'l ',
      'il',
      'ni',
      'en',
      '.e',
      ' .',
    }
    assert.are.same expected, transpose input

  it 'mixed line length', ->
    input = {
      'The longest line.',
      'A long line.',
      'A longer line.',
      'A line.',
    }
    expected = {
      'TAAA',
      'h   ',
      'elll',
      ' ooi',
      'lnnn',
      'ogge',
      'n e.',
      'glr',
      'ei ',
      'snl',
      'tei',
      ' .n',
      'l e',
      'i .',
      'n',
      'e',
      '.',
    }
    assert.are.same expected, transpose input

  it 'square', ->
    input = {
      'HEART',
      'EMBER',
      'ABUSE',
      'RESIN',
      'TREND',
    }
    expected = {
      'HEART',
      'EMBER',
      'ABUSE',
      'RESIN',
      'TREND',
    }
    assert.are.same expected, transpose input

  it 'rectangle', ->
    input = {
      'FRACTURE',
      'OUTLINED',
      'BLOOMING',
      'SEPTETTE',
    }
    expected = {
      'FOBS',
      'RULE',
      'ATOP',
      'CLOT',
      'TIME',
      'UNIT',
      'RENT',
      'EDGE',
    }
    assert.are.same expected, transpose input

  it 'triangle', ->
    input = {
      'T',
      'EE',
      'AAA',
      'SSSS',
      'EEEEE',
      'RRRRRR',
    }
    expected = {
      'TEASER',
      ' EASER',
      '  ASER',
      '   SER',
      '    ER',
      '     R',
    }
    assert.are.same expected, transpose input

  it 'jagged triangle', ->
    input = {
      '11',
      '2',
      '3333',
      '444',
      '555555',
      '66666',
    }
    expected = {
      '123456',
      '1 3456',
      '  3456',
      '  3 56',
      '    56',
      '    5',
    }
    assert.are.same expected, transpose input
