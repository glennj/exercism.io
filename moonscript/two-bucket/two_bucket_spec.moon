import measure from require 'two_bucket'

describe 'two-bucket', ->
  it 'Measure using bucket one of size 3 and bucket two of size 5 - start with bucket one', ->
    result = measure bucketOne: 3, bucketTwo: 5, goal: 1, startBucket: 'one'
    expected = moves: 4, goalBucket: 'one', otherBucket: 5
    assert.are.same expected, result

  it 'Measure using bucket one of size 3 and bucket two of size 5 - start with bucket two', ->
    result = measure bucketOne: 3, bucketTwo: 5, goal: 1, startBucket: 'two'
    expected = moves: 8, goalBucket: 'two', otherBucket: 3
    assert.are.same expected, result

  it 'Measure using bucket one of size 7 and bucket two of size 11 - start with bucket one', ->
    result = measure bucketOne: 7, bucketTwo: 11, goal: 2, startBucket: 'one'
    expected = moves: 14, goalBucket: 'one', otherBucket: 11
    assert.are.same expected, result

  it 'Measure using bucket one of size 7 and bucket two of size 11 - start with bucket two', ->
    result = measure bucketOne: 7, bucketTwo: 11, goal: 2, startBucket: 'two'
    expected = moves: 18, goalBucket: 'two', otherBucket: 7
    assert.are.same expected, result

  it 'Measure one step using bucket one of size 1 and bucket two of size 3 - start with bucket two', ->
    result = measure bucketOne: 1, bucketTwo: 3, goal: 3, startBucket: 'two'
    expected = moves: 1, goalBucket: 'two', otherBucket: 0
    assert.are.same expected, result

  it 'Measure using bucket one of size 2 and bucket two of size 3 - start with bucket one and end with bucket two', ->
    result = measure bucketOne: 2, bucketTwo: 3, goal: 3, startBucket: 'one'
    expected = moves: 2, goalBucket: 'two', otherBucket: 2
    assert.are.same expected, result

  it 'Measure using bucket one much bigger than bucket two', ->
    result = measure bucketOne: 5, bucketTwo: 1, goal: 2, startBucket: 'one'
    expected = moves: 6, goalBucket: 'one', otherBucket: 1
    assert.are.same expected, result

  it 'Measure using bucket one much smaller than bucket two', ->
    result = measure bucketOne: 3, bucketTwo: 15, goal: 9, startBucket: 'one'
    expected = moves: 6, goalBucket: 'two', otherBucket: 0
    assert.are.same expected, result

  it 'Not possible to reach the goal', ->
    assert.has.errors -> measure bucketOne: 6, bucketTwo: 15, goal: 5, startBucket: 'one'

  it 'With the same buckets but a different goal, then it is possible', ->
    result = measure bucketOne: 6, bucketTwo: 15, goal: 9, startBucket: 'one'
    expected = moves: 10, goalBucket: 'two', otherBucket: 0
    assert.are.same expected, result

  it 'Goal larger than both buckets is impossible', ->
    assert.has.errors -> measure bucketOne: 5, bucketTwo: 7, goal: 8, startBucket: 'one'
