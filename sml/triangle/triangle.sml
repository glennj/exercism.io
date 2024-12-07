use "list-utils.sml";   (* uniqReal *)
use "sorting.sml";      (* sortBy *)

local
  val sortAscending = sortBy Real.<=

  fun valid (sides as [_, _, _]: real list): bool =
        let val [a, b, c] = sortAscending sides
        in  a > 0.0 andalso a + b >= c
        end
    | valid _ = raise Domain

  fun triangle sides cmp n =
    (valid sides) andalso cmp (length (uniqReal sides), n)

in
  fun equilateral (sides: real list): bool = triangle sides op=  1  
  fun isosceles   (sides: real list): bool = triangle sides op<= 2
  fun scalene     (sides: real list): bool = triangle sides op=  3
end
