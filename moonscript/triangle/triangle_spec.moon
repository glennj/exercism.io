import is_equilateral, is_isosceles, is_scalene from require 'triangle'

describe 'triangle', ->
  describe 'equilateral triangle', ->
    it 'all sides are equal', ->
      assert.is_true is_equilateral(2, 2, 2)

    it 'any side is unequal', ->
      assert.is_false is_equilateral(2, 3, 2)

    it 'no sides are equal', ->
      assert.is_false is_equilateral(5, 4, 6)

    it 'all zero sides is not a triangle', ->
      assert.is_false is_equilateral(0, 0, 0)

    it 'sides may be floats', ->
      assert.is_true is_equilateral(0.5, 0.5, 0.5)

  describe 'isosceles triangle', ->
    it 'last two sides are equal', ->
      assert.is_true is_isosceles(3, 4, 4)

    it 'first two sides are equal', ->
      assert.is_true is_isosceles(4, 4, 3)

    it 'first and last sides are equal', ->
      assert.is_true is_isosceles(4, 3, 4)

    it 'equilateral triangles are also isosceles', ->
      assert.is_true is_isosceles(4, 4, 4)

    it 'no sides are equal', ->
      assert.is_false is_isosceles(2, 3, 4)

    it 'first triangle inequality violation', ->
      assert.is_false is_isosceles(1, 1, 3)

    it 'second triangle inequality violation', ->
      assert.is_false is_isosceles(1, 3, 1)

    it 'third triangle inequality violation', ->
      assert.is_false is_isosceles(3, 1, 1)

    it 'sides may be floats', ->
      assert.is_true is_isosceles(0.5, 0.4, 0.5)

  describe 'scalene triangle', ->
    it 'no sides are equal', ->
      assert.is_true is_scalene(5, 4, 6)

    it 'all sides are equal', ->
      assert.is_false is_scalene(4, 4, 4)

    it 'first and second sides are equal', ->
      assert.is_false is_scalene(4, 4, 3)

    it 'first and third sides are equal', ->
      assert.is_false is_scalene(3, 4, 3)

    it 'second and third sides are equal', ->
      assert.is_false is_scalene(4, 3, 3)

    it 'may not violate triangle inequality', ->
      assert.is_false is_scalene(7, 3, 2)

    it 'sides may be floats', ->
      assert.is_true is_scalene(0.5, 0.4, 0.6)
