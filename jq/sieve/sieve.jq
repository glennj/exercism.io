def make_sieve:
  [range(. + 1)]
  | .[0] = false
  | .[1] = false
;

def mark_multiples(n):
  if .[n]
  then
    length as $limit
    | . as $sieve
    | (if n == 2 then n else 2 * n end) as $step
    | reduce range(n * n; $limit; $step) as $i ($sieve; .[$i] = false)
  else .
  end
;

def eliminate_multiples:
  reduce (2, range(3; length | sqrt; 2)) as $p (.; mark_multiples($p))
;

def extract_primes: map(select(.));

.limit | make_sieve | eliminate_multiples | extract_primes
