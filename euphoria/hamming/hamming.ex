include std/sequence.e

public function distance(sequence left, sequence right)
  if length(left) != length(right) then
    return "left and right strands must be of equal length"
  end if

  /* -- an iterative approach
    integer dist = 0
    for i = 1 to length(left) do
      if left[i] != right[i] then
        dist += 1
      end if
    end for
    return dist
  */

  -- a more functional approach
  return length(filter(zip(left, right), routine_id("neq")))
end function

/* combine two sequences pairwise:
 *    zip("Hello", "World") --> {"HW", "eo", "lr", "ll", "od"}
 */
function zip(sequence a, sequence b)
  return columnize({a, b})
end function

/* given a 2-element sequence, are the elements _not_ equal
 */
function neq(sequence pair, object _)
  return not equal(pair[1], pair[2])
end function
