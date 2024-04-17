include std/sequence.e

public function primes(integer limit)
  sequence marks = series(1, 1, limit)
  marks[1] = 0

  marks = mark_multiples(marks, 2, limit, 2)
  for p = 3 to floor(sqrt(limit)) by 2 do
    if marks[p] then
      marks = mark_multiples(marks, p, limit, 2*p)
    end if
  end for

  return remove_all(0, marks)
end function

function mark_multiples(sequence marks, integer prime, integer limit, integer step)
  for m = prime * prime to limit by step do
    marks[m] = 0
  end for
  return marks
end function
