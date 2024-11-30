local
  fun eq wanted c = c = wanted

  fun matrixFromString str =
    let val lines  = String.tokens (eq #"\n") str
        val strMtx = List.map (String.tokens (eq #" ")) lines
    in  List.map (List.map (valOf o Int.fromString)) strMtx
    end

in
  fun row (s: string, index: int): int list =
    List.nth (matrixFromString s, index - 1)

  fun column (s: string, index: int): int list =
    List.map (fn row => List.nth (row, index - 1)) (matrixFromString s)
end
