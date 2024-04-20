include std/math.e
include std/sort.e
include std/sequence.e

public function latest(sequence scores)
  return scores[$]
end function

public function personal_best(sequence scores)
  return max(scores)
end function

public function personal_top_three(sequence scores)
  return slice(sort(scores, stdsort:DESCENDING), 1, 3)
end function
