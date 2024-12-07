use "int-utils.sml";    (* intReverse *)

local
  fun isPalindrome n = n = intReverse n

  fun boundedFactors (min, max) num = bf' num min max []
  and bf' n f max fs =
    if f * f > n orelse f > max then List.rev fs
    else if n mod f <> 0 then bf' n (f + 1) max fs
    else let val g = n div f
          in  if g > max 
              then bf' n (f + 1) max fs
              else bf' n (f + 1) max ((f, g) :: fs)
          end

  fun solve (bounds as (min, max)) n limit step =
    if min > max then raise Fail "min must be <= max"
    else s' bounds n limit step

  and s' bounds n limit step =
    if limit n then NONE
    else if not (isPalindrome n) then s' bounds (n + step) limit step
    else let val fs = boundedFactors bounds n
         in  if length fs = 0
             then s' bounds (n + step) limit step
             else SOME {value = n, factors = fs}
         end

in
  fun smallest (min: int, max: int): {value: int, factors: (int * int) list} option =
    solve (min, max) (min * min) (fn n => n > max * max) 1

  fun largest (min: int, max: int): {value: int, factors: (int * int) list} option =
    solve (min, max) (max * max) (fn n => n < min * min) ~1
end
