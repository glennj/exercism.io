datatype queen = Queen of int * int
fun rank (Queen (r, _)) = r
fun file (Queen (_, c)) = c

local
  fun check what n =
    if      n  < 0 then raise Fail (what ^ " not positive")
    else if n >= 8 then raise Fail (what ^ " not on board")
    else n
in
  fun create (rank: int, file: int): queen =
    Queen (check "row" rank, check "column" file)

  fun canAttack (whiteQueen: queen) (blackQueen: queen): bool =
    let val dr = abs (rank whiteQueen - rank blackQueen)
        val df = abs (file whiteQueen - file blackQueen)
    in  dr = 0 orelse df = 0 orelse dr = df
    end
end
