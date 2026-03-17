FlowerField = require 'flower_field'

describe 'flower-field', ->
  it 'no rows', ->
    garden = {}
    expected = {}
    result = FlowerField.annotate garden
    assert.are.same expected, result

  it 'no columns', ->
    garden = {
      ''
    }
    expected = {
      ''
    }
    result = FlowerField.annotate garden
    assert.are.same expected, result

  it 'no flowers', ->
    garden = {
      '   '
      '   '
      '   '
    }
    expected = {
      '   '
      '   '
      '   '
    }
    result = FlowerField.annotate garden
    assert.are.same expected, result

  it 'garden full of flowers', ->
    garden = {
      '***'
      '***'
      '***'
    }
    expected = {
      '***'
      '***'
      '***'
    }
    result = FlowerField.annotate garden
    assert.are.same expected, result

  it 'flower surrounded by spaces', ->
    garden = {
      '   '
      ' * '
      '   '
    }
    expected = {
      '111'
      '1*1'
      '111'
    }
    result = FlowerField.annotate garden
    assert.are.same expected, result

  it 'space surrounded by flowers', ->
    garden = {
      '***'
      '* *'
      '***'
    }
    expected = {
      '***'
      '*8*'
      '***'
    }
    result = FlowerField.annotate garden
    assert.are.same expected, result

  it 'horizontal line', ->
    garden = {
      ' * * '
    }
    expected = {
      '1*2*1'
    }
    result = FlowerField.annotate garden
    assert.are.same expected, result

  it 'horizontal line, flowers at edges', ->
    garden = {
      '*   *'
    }
    expected = {
      '*1 1*'
    }
    result = FlowerField.annotate garden
    assert.are.same expected, result

  it 'vertical line', ->
    garden = {
      ' '
      '*'
      ' '
      '*'
      ' '
    }
    expected = {
      '1'
      '*'
      '2'
      '*'
      '1'
    }
    result = FlowerField.annotate garden
    assert.are.same expected, result

  it 'vertical line, flowers at edges', ->
    garden = {
      '*'
      ' '
      ' '
      ' '
      '*'
    }
    expected = {
      '*'
      '1'
      ' '
      '1'
      '*'
    }
    result = FlowerField.annotate garden
    assert.are.same expected, result

  it 'cross', ->
    garden = {
      '  *  '
      '  *  '
      '*****'
      '  *  '
      '  *  '
    }
    expected = {
      ' 2*2 '
      '25*52'
      '*****'
      '25*52'
      ' 2*2 '
    }
    result = FlowerField.annotate garden
    assert.are.same expected, result

  it 'large garden', ->
    garden = {
      ' *  * '
      '  *   '
      '    * '
      '   * *'
      ' *  * '
      '      '
    }
    expected = {
      '1*22*1'
      '12*322'
      ' 123*2'
      '112*4*'
      '1*22*2'
      '111111'
    }
    result = FlowerField.annotate garden
    assert.are.same expected, result

  it 'multiple adjacent flowers', ->
    garden = {
      ' ** '
    }
    expected = {
      '1**1'
    }
    result = FlowerField.annotate garden
    assert.are.same expected, result
