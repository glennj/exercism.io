enum DISCARD=0, KEEP

type r_id(integer id)
  return id >= 0
end type

public function keep(sequence seq, r_id predicate)
  return strain(seq, predicate, KEEP)
end function

public function discard(sequence seq, r_id predicate)
  return strain(seq, predicate, DISCARD)
end function

function strain(sequence seq, r_id predicate, integer keep)
  sequence result = {}
  for i = 1 to length(seq) do
    integer value = call_func(predicate, {seq[i]})
    if (value and keep) or (not value and not keep) then    -- alternately, "not (a xor b)"
      result = append(result, seq[i])
    end if
  end for
  return result
end function
