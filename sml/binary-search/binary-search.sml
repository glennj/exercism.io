fun find (haystack: int array, needle: int): int =
  let fun find' left right =
        if left > right then raise Fail "value not in array"
        else let val mid  = (left + right) div 2
                 val elem = Array.sub (haystack, mid)
             in  if      needle < elem then find' left (mid - 1)
                 else if needle > elem then find' (mid + 1) right
                 else mid
             end
  in  find' 0 (Array.length haystack - 1)
  end
  