local
  fun nextPrime (prevPrime: int): (bool array -> int option) =
    (Option.map #1) o (Array.findi (fn (i, b) => b andalso i > prevPrime))

  fun markMultiples (ary: bool array, limit: int, prime: int option): unit =
    case prime
      of NONE   => ()
       | SOME p => 
            (* gnarly nested functions here *)
            let fun mark' (p', step) =
              let fun m' multiple =
                    if multiple > limit then ()
                    else ( Array.update (ary, multiple, false);
                           m' (multiple + step) )
              in  if not (Array.sub (ary, p')) then ()
                  else m' (p' * p')
              end
            in
               if p = 2 then mark' (2, 2) else mark' (p, 2 * p);
               markMultiples (ary, limit, nextPrime p ary)
            end

in
  fun primes (limit: int): int list =
    let val A  = Array.tabulate (limit + 1, fn i => i >= 2)
        val () = markMultiples (A, limit, nextPrime 0 A)
    in  Array.foldri (fn (_, false, ps) => ps | (p, true, ps) => p :: ps) [] A
    end
end
