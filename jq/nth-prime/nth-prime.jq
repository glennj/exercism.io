# this is much faster then doing a `select` on the entire $primes array
def is_prime($primes):
  def _is_prime:
    if $primes[.idx] > .limit then true
    elif .candidate % $primes[.idx] == 0 then false
    else .idx += 1 | _is_prime
    end
  ;
  {candidate: ., limit: sqrt, idx: 0} | _is_prime
;

def next_prime($primes):
  def _next_prime:
    if is_prime($primes)
      then .
      else (. + 2) | _next_prime
    end
  ;
  ($primes[-1] + 2) | _next_prime
;

def nthprime(n):
  def _nthprime:
    if length == n
      then last
      else . + [next_prime(.)] | _nthprime
    end
  ;
  _nthprime
;

if $n == 0 then "there is no zeroth prime" | halt_error
elif $n == 1 then 2
else [2, 3] | nthprime($n)
end
