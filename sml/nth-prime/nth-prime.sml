local
  val primes = ref [2, 3]

  fun isPrime c =
    let fun prime' [] c      = true
          | prime' (p::ps) c =
              if p * p > c        then true
              else if c mod p = 0 then false
              else prime' ps c
    in  prime' (!primes) c
    end
      
  fun nextPrime () =
    let fun next' p = if isPrime p then p else next' (p + 2)
    in  next' (List.last (!primes) + 2)
    end
in
  fun nthPrime n =
    if n < 1                      then NONE
    else if length (!primes) >= n then SOME (List.nth (!primes, n - 1))
    else let val () = primes := (!primes) @ [nextPrime ()]
         in  nthPrime n
         end
end
