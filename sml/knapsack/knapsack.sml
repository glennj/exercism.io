(* Translating the pseudocode from https://en.wikipedia.org/wiki/Knapsack_problem
 *
 *  Assume w_{1}, w_{2}, ..., w_{n}, W are strictly positive integers. 
 *  Define m[i,w] to be the maximum value that can be attained with weight
 *  less than or equal to w using items up to i (first i items).
 * 
 *  We can define m[i,w] recursively as follows: 
 *  * m[0,w] = 0
 *  * m[i,w] = m[i-1,w] if w_{i} > w (the new item is more than the current weight limit)
 *  * m[i,w] = max(m[i-1,w], m[i-1,w-w_{i}]+v_{i}) if w_{i} <= w.
 *
 * The solution can then be found by calculating m[n,W]. 
 *)

fun maximumValue (items: {value: int, weight: int} list, maximumWeight: int): int =
  let val n   = length items
      val m   = Array2.array (n + 1, maximumWeight + 1, 0)
      val reg = {base = m, row = 0, col = 0, nrows = NONE, ncols = NONE}
      val f   = fn (i, w, a) =>
                  if w = 0 orelse i = 0 then a
                  else let val {weight = W, value = V} = List.nth (items, i - 1)
                       in  if W > w then Array2.sub (m, i - 1, w)
                           else Int.max ( Array2.sub (m, i - 1, w),
                                          Array2.sub (m, i - 1, w - W) + V )
                       end
  in  Array2.modifyi Array2.RowMajor f reg;
      Array2.sub (m, n, maximumWeight)
  end
