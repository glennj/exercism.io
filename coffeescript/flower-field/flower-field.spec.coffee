FlowerField = require './flower-field'

describe 'Flower Field', ->
  it 'no rows', ->
    garden = []
    expect(FlowerField.annotate garden).toEqual []

  it 'no columns', ->
    garden = ['']
    expect(FlowerField.annotate garden).toEqual ['']

  it 'no flowers', ->
    garden = [
      '   '
      '   '
      '   '
    ]
    expect(FlowerField.annotate garden).toEqual [
      '   '
      '   '
      '   '
    ]

  it 'garden full of flowers', ->
    garden = [
      '***'
      '***'
      '***'
    ]
    expect(FlowerField.annotate garden).toEqual [
      '***'
      '***'
      '***'
    ]

  it 'flower surrounded by spaces', ->
    garden = [
      '   '
      ' * '
      '   '
    ]
    expect(FlowerField.annotate garden).toEqual [
      '111'
      '1*1'
      '111'
    ]

  it 'space surrounded by flowers', ->
    garden = [
      '***'
      '* *'
      '***'
    ]
    expect(FlowerField.annotate garden).toEqual [
      '***'
      '*8*'
      '***'
    ]

  it 'horizontal line', ->
    garden = [' * * ']
    expect(FlowerField.annotate garden).toEqual ['1*2*1']

  it 'horizontal line, flowers at edges', ->
    garden = ['*   *']
    expect(FlowerField.annotate garden).toEqual ['*1 1*']

  it 'vertical line', ->
    garden = [
      ' '
      '*'
      ' '
      '*'
      ' '
    ]
    expect(FlowerField.annotate garden).toEqual [
      '1'
      '*'
      '2'
      '*'
      '1'
    ]

  it 'vertical line, flowers at edges', ->
    garden = [
      '*'
      ' '
      ' '
      ' '
      '*'
    ]
    expect(FlowerField.annotate garden).toEqual [
      '*'
      '1'
      ' '
      '1'
      '*'
    ]

  it 'cross', ->
    garden = [
      '  *  '
      '  *  '
      '*****'
      '  *  '
      '  *  '
    ]
    expect(FlowerField.annotate garden).toEqual [
      ' 2*2 '
      '25*52'
      '*****'
      '25*52'
      ' 2*2 '
    ]

  it 'large garden', ->
    garden = [
      ' *  * '
      '  *   '
      '    * '
      '   * *'
      ' *  * '
      '      '
    ]
    expect(FlowerField.annotate garden).toEqual [
      '1*22*1'
      '12*322'
      ' 123*2'
      '112*4*'
      '1*22*2'
      '111111'
    ]
