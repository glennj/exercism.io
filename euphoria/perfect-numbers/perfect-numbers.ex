type positive_integer(atom number)
  return integer(number) and number > 0
end type

public function classify(atom n)
  if not positive_integer(n) then
    return 0
  end if

  switch compare(aliquot_sum(n), n) do
    case -1 then return "deficient"
    case  0 then return "perfect"
    case  1 then return "abundant"
  end switch
end function

function aliquot_sum(atom n)
  atom sum = 0
  for f = 1 to floor(sqrt(n)) do
    if remainder(n, f) = 0 then
      sum += f
      if f != n / f then
        sum += n / f
      end if
    end if
  end for
  return sum - n
end function
