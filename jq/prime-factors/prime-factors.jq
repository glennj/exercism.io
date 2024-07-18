def prime_factors:
  if   .f * .f > .n then if .n > 1 then .p += [.n] end | .p
  elif .n % .f == 0 then .n /= .f | .p += [.f] | prime_factors
  else                   .f += 1 | prime_factors
  end
;

{n: .value, f: 2, p: []} | prime_factors