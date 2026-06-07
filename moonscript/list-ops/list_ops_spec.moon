ListOps = require 'list_ops'

describe 'list-ops:', ->
  describe 'append entries to a list and return the new list:', ->
    it 'empty lists', ->
      result = ListOps.append {}, {}
      expected = {}
      assert.are.same expected, result

    it 'list to empty list', ->
      result = ListOps.append {}, {1, 2, 3, 4}
      expected = {1, 2, 3, 4}
      assert.are.same expected, result

    it 'empty list to list', ->
      result = ListOps.append {1, 2, 3, 4}, {}
      expected = {1, 2, 3, 4}
      assert.are.same expected, result

    it 'non-empty lists', ->
      result = ListOps.append {1, 2}, {2, 3, 4, 5}
      expected = {1, 2, 2, 3, 4, 5}
      assert.are.same expected, result

  describe 'concatenate a list of lists:', ->
    it 'empty list', ->
      result = ListOps.concat {}
      expected = {}
      assert.are.same expected, result

    it 'list of lists', ->
      result = ListOps.concat {{1, 2}, {3}, {}, {4, 5, 6}}
      expected = {1, 2, 3, 4, 5, 6}
      assert.are.same expected, result

    it 'list of nested lists', ->
      result = ListOps.concat {{{1}, {2}}, {{3}}, {{}}, {{4, 5, 6}}}
      expected = {{1}, {2}, {3}, {}, {4, 5, 6}}
      assert.are.same expected, result

  describe 'filter list returning only values that satisfy the filter function:', ->
    it 'empty list', ->
      result = ListOps.filter {}, (x) -> x % 2 == 1
      expected = {}
      assert.are.same expected, result

    it 'non-empty list', ->
      result = ListOps.filter {1, 2, 3, 5}, (x) -> x % 2 == 1
      expected = {1, 3, 5}
      assert.are.same expected, result

  describe 'returns the length of a list:', ->
    it 'empty list', ->
      result = ListOps.length {}
      expected = 0
      assert.are.equal expected, result

    it 'non-empty list', ->
      result = ListOps.length {1, 2, 3, 4}
      expected = 4
      assert.are.equal expected, result

  describe 'return a list of elements whose values equal the list value transformed by the mapping function:', ->
    it 'empty list', ->
      result = ListOps.map {}, (x) -> x + 1
      expected = {}
      assert.are.same expected, result

    it 'non-empty list', ->
      result = ListOps.map {1, 3, 5, 7}, (x) -> x + 1
      expected = {2, 4, 6, 8}
      assert.are.same expected, result

  describe 'folds (reduces) the given list from the left with a function:', ->
    it 'empty list', ->
      result = ListOps.foldl {}, 2, (acc, el) -> el * acc
      expected = 2
      assert.are.equal expected, result

    it 'direction independent function applied to non-empty list', ->
      result = ListOps.foldl {1, 2, 3, 4}, 5, (acc, el) -> el + acc
      expected = 15
      assert.are.equal expected, result

    it 'direction dependent function applied to non-empty list', ->
      result = ListOps.foldl {1, 2, 3, 4}, 24, (acc, el) -> el / acc
      expected = 64
      assert.are.equal expected, result

  describe 'folds (reduces) the given list from the right with a function:', ->
    it 'empty list', ->
      result = ListOps.foldr {}, 2, (acc, el) -> el * acc
      expected = 2
      assert.are.equal expected, result

    it 'direction independent function applied to non-empty list', ->
      result = ListOps.foldr {1, 2, 3, 4}, 5, (acc, el) -> el + acc
      expected = 15
      assert.are.equal expected, result

    it 'direction dependent function applied to non-empty list', ->
      result = ListOps.foldr {1, 2, 3, 4}, 24, (acc, el) -> el / acc
      expected = 9
      assert.are.equal expected, result

  describe 'reverse the elements of the list:', ->
    it 'empty list', ->
      result = ListOps.reverse {}
      expected = {}
      assert.are.same expected, result

    it 'non-empty list', ->
      result = ListOps.reverse {1, 3, 5, 7}
      expected = {7, 5, 3, 1}
      assert.are.same expected, result

    it 'list of lists is not flattened', ->
      result = ListOps.reverse {{1, 2}, {3}, {}, {4, 5, 6}}
      expected = {{4, 5, 6}, {}, {3}, {1, 2}}
      assert.are.same expected, result
