fun tripletsWithSum (n: int): (int * int * int) list =
  let fun findTriplets (a, triplets) =
    (* formuala for b is derived from:
     *    a + b + c = n     => c^2 = (n - a - b)^2
     *    a^2 + b^2 = c^2   => a^2 + b^2 = (n - a - b)^2
     *)
    let val b = n * (n - 2 * a) div (2 * (n - a))
        val c = n - a - b
    in  if a >= b
        then (List.rev triplets)
        else if a * a + b * b = c * c
             then findTriplets (a + 1, (a, b, c) :: triplets)
             else findTriplets (a + 1, triplets)
    end
  in
    (* magic number alert:
     * 3 is the shortest side of the smallest Pythangorean triplet (3, 4, 5)
     *)
    findTriplets (3, [])
  end
