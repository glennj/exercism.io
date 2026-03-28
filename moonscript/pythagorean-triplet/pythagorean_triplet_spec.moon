import triplets_with_sum from require 'pythagorean_triplet'

describe 'pythagorean-triplet', ->
  it 'triplets whose sum is 12', ->
    result = triplets_with_sum 12
    expected = {{3, 4, 5}}
    assert.are.same expected, result

  pending 'triplets whose sum is 108', ->
    result = triplets_with_sum 108
    expected = {{27, 36, 45}}
    assert.are.same expected, result

  pending 'triplets whose sum is 1000', ->
    result = triplets_with_sum 1000
    expected = {{200, 375, 425}}
    assert.are.same expected, result

  pending 'no matching triplets for 1001', ->
    result = triplets_with_sum 1001
    expected = {}
    assert.are.same expected, result

  pending 'returns all matching triplets', ->
    result = triplets_with_sum 90
    expected = {
      {9, 40, 41}
      {15, 36, 39}
    }
    assert.are.same expected, result

  pending 'several matching triplets', ->
    result = triplets_with_sum 840
    expected = {
      {40, 399, 401}
      {56, 390, 394}
      {105, 360, 375}
      {120, 350, 370}
      {140, 336, 364}
      {168, 315, 357}
      {210, 280, 350}
      {240, 252, 348}
    }
    assert.are.same expected, result

  pending 'triplets for large number', ->
    result = triplets_with_sum 30000
    expected = {
      {1200, 14375, 14425}
      {1875, 14000, 14125}
      {5000, 12000, 13000}
      {6000, 11250, 12750}
      {7500, 10000, 12500}
    }
    assert.are.same expected, result
