Diamond = require './diamond'

describe 'Diamond', ->
  it 'Degenerate case with a single A row', ->
    expected = [
      'A'
    ].join '\n'
    expect(Diamond.rows 'A').toEqual expected

  it 'Degenerate case with no row containing 3 distinct groups of spaces', ->
    expected = [
      ' A '
      'B B'
      ' A '
    ].join '\n'
    expect(Diamond.rows 'B').toEqual expected

  it 'Smallest non-degenerate case with odd diamond side length', ->
    expected = [
      '  A  '
      ' B B '
      'C   C'
      ' B B '
      '  A  '
    ].join '\n'
    expect(Diamond.rows 'C').toEqual expected

  it 'Smallest non-degenerate case with even diamond side length', ->
    expected = [
      '   A   '
      '  B B  '
      ' C   C '
      'D     D'
      ' C   C '
      '  B B  '
      '   A   '
    ].join '\n'
    expect(Diamond.rows 'D').toEqual expected

  it 'Largest possible diamond', ->
    expected = [
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
    ].join '\n'
    expect(Diamond.rows 'Z').toEqual expected
