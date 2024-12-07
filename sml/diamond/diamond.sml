use "list-utils.sml";   (* seq *)

local
  val A = Char.ord #"A"
  
  fun populate (arr, n, size, i): unit =
    if i = n then ()
    else let val row1 = i
             val row2 = size - 1 - i
             val col1 = n - 1 + i
             val col2 = n - 1 - i
             val letter = Char.chr (A + i)
         in  Array2.update (arr, row1, col1, letter) ;
             Array2.update (arr, row1, col2, letter) ;
             Array2.update (arr, row2, col1, letter) ;
             Array2.update (arr, row2, col2, letter) ;
             populate (arr, n, size, i + 1)
         end

  fun array2MapRows (f: 'a vector -> 'b) (arr: 'a Array2.array): 'b list =
    List.map (fn i => f (Array2.row (arr, i))) (seq (Array2.nRows arr))

  fun charVectorToString (vec: char vector): string =
    Vector.foldr (fn (c, acc) => str c ^ acc) "" vec

in
  fun rows (input: string): string list =
    let val n = (Char.ord o valOf o Char.fromString) input - A + 1
        val size = 2 * n - 1
        val arr = Array2.array (size, size, #" ")
    in  populate (arr, n, size, 0);
        array2MapRows charVectorToString arr
    end
end
