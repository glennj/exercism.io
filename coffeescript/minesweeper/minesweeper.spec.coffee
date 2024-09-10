Minesweeper = require './minesweeper'

describe 'Minesweeper', ->
  it 'no rows', ->
    minefield = []
    expect(Minesweeper.annotate minefield).toEqual []

  it 'no columns', ->
    minefield = ['']
    expect(Minesweeper.annotate minefield).toEqual ['']

  it 'no mines', ->
    minefield = [
      '   '
      '   '
      '   '
    ]
    expect(Minesweeper.annotate minefield).toEqual [
      '   '
      '   '
      '   '
    ]

  it 'minefield with only mines', ->
    minefield = [
      '***'
      '***'
      '***'
    ]
    expect(Minesweeper.annotate minefield).toEqual [
      '***'
      '***'
      '***'
    ]

  it 'mine surrounded by spaces', ->
    minefield = [
      '   '
      ' * '
      '   '
    ]
    expect(Minesweeper.annotate minefield).toEqual [
      '111'
      '1*1'
      '111'
    ]

  it 'space surrounded by mines', ->
    minefield = [
      '***'
      '* *'
      '***'
    ]
    expect(Minesweeper.annotate minefield).toEqual [
      '***'
      '*8*'
      '***'
    ]

  it 'horizontal line', ->
    minefield = [' * * ']
    expect(Minesweeper.annotate minefield).toEqual ['1*2*1']

  it 'horizontal line, mines at edges', ->
    minefield = ['*   *']
    expect(Minesweeper.annotate minefield).toEqual ['*1 1*']

  it 'vertical line', ->
    minefield = [
      ' '
      '*'
      ' '
      '*'
      ' '
    ]
    expect(Minesweeper.annotate minefield).toEqual [
      '1'
      '*'
      '2'
      '*'
      '1'
    ]

  it 'vertical line, mines at edges', ->
    minefield = [
      '*'
      ' '
      ' '
      ' '
      '*'
    ]
    expect(Minesweeper.annotate minefield).toEqual [
      '*'
      '1'
      ' '
      '1'
      '*'
    ]

  it 'cross', ->
    minefield = [
      '  *  '
      '  *  '
      '*****'
      '  *  '
      '  *  '
    ]
    expect(Minesweeper.annotate minefield).toEqual [
      ' 2*2 '
      '25*52'
      '*****'
      '25*52'
      ' 2*2 '
    ]

  it 'large minefield', ->
    minefield = [
      ' *  * '
      '  *   '
      '    * '
      '   * *'
      ' *  * '
      '      '
    ]
    expect(Minesweeper.annotate minefield).toEqual [
      '1*22*1'
      '12*322'
      ' 123*2'
      '112*4*'
      '1*22*2'
      '111111'
    ]
