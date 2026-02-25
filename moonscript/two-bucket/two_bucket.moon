import gcd from require 'lib/math'


class Bucket
  new: (name, size) =>
    @name = name
    @size = size
    @amount = 0

  is_full:  => @amount == @size
  is_empty: => @amount == 0
  fill:  => @amount = @size
  empty: => @amount = 0

  pour_into: (other) =>
    quantity = math.min @amount, (other.size - other.amount)
    @amount -= quantity
    other.amount += quantity


validate = (params) ->
  {bucketOne: b1, bucketTwo: b2, :goal} = params
  if goal > math.max(b1, b2) then return false
  div = gcd b1, b2
  if div > 1 and goal % div != 0 then return false
  true


solve = (start, other, goal) ->
  moves = 0

  start\fill!
  moves += 1

  if other.size == goal and start.size != goal
    other\fill!
    moves += 1

  while true
    if start.amount == goal
      return {:moves, goalBucket: start.name, otherBucket: other.amount}
    if other.amount == goal
      return {:moves, goalBucket: other.name, otherBucket: start.amount}

    if     start\is_empty! then start\fill!
    elseif other\is_full!  then other\empty!
    else                        start\pour_into other

    moves += 1


{
  measure: (params) ->
    assert validate(params), 'cannot reach goal'

    b1 = Bucket 'one', params.bucketOne
    b2 = Bucket 'two', params.bucketTwo

    switch params.startBucket
      when 'one' then solve b1, b2, params.goal
      when 'two' then solve b2, b1, params.goal
}
