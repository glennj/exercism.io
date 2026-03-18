Diamond = require 'diamond'

describe 'diamond', ->
  it "Degenerate case with a single 'A' row", ->
    result = Diamond.rows 'A'
    expected = {
      'A'
    }
    assert.are.same expected, result

  it 'Degenerate case with no row containing 3 distinct groups of spaces', ->
    result = Diamond.rows 'B'
    expected = {
      ' A '
      'B B'
      ' A '
    }
    assert.are.same expected, result

  it 'Smallest non-degenerate case with odd diamond side length', ->
    result = Diamond.rows 'C'
    expected = {
      '  A  '
      ' B B '
      'C   C'
      ' B B '
      '  A  '
    }
    assert.are.same expected, result

  it 'Smallest non-degenerate case with even diamond side length', ->
    result = Diamond.rows 'D'
    expected = {
      '   A   '
      '  B B  '
      ' C   C '
      'D     D'
      ' C   C '
      '  B B  '
      '   A   '
    }
    assert.are.same expected, result

  it 'Largest possible diamond', ->
    result = Diamond.rows 'Z'
    expected = {
      '                         A                         '
      '                        B B                        '
      '                       C   C                       '
      '                      D     D                      '
      '                     E       E                     '
      '                    F         F                    '
      '                   G           G                   '
      '                  H             H                  '
      '                 I               I                 '
      '                J                 J                '
      '               K                   K               '
      '              L                     L              '
      '             M                       M             '
      '            N                         N            '
      '           O                           O           '
      '          P                             P          '
      '         Q                               Q         '
      '        R                                 R        '
      '       S                                   S       '
      '      T                                     T      '
      '     U                                       U     '
      '    V                                         V    '
      '   W                                           W   '
      '  X                                             X  '
      ' Y                                               Y '
      'Z                                                 Z'
      ' Y                                               Y '
      '  X                                             X  '
      '   W                                           W   '
      '    V                                         V    '
      '     U                                       U     '
      '      T                                     T      '
      '       S                                   S       '
      '        R                                 R        '
      '         Q                               Q         '
      '          P                             P          '
      '           O                           O           '
      '            N                         N            '
      '             M                       M             '
      '              L                     L              '
      '               K                   K               '
      '                J                 J                '
      '                 I               I                 '
      '                  H             H                  '
      '                   G           G                   '
      '                    F         F                    '
      '                     E       E                     '
      '                      D     D                      '
      '                       C   C                       '
      '                        B B                        '
      '                         A                         '
    }
    assert.are.same expected, result
