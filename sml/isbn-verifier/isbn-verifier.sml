local
  fun isbnSum [] m sum = (* check for too short *)
        if m > 0 then NONE else SOME sum
    | isbnSum ns 0 sum = (* check for too long *)
        if length ns > 0 then NONE else SOME sum
    | isbnSum (n::ns) m sum =
        if Char.isDigit n
        then isbnSum ns (m - 1) (sum + m * (ord n - ord #"0"))
        else if m = 1 andalso n = #"X"
             then SOME (sum + 10)
             else NONE

  val isbnDigits = (List.filter (fn c => c <> #"-")) o explode

in
  fun isValid (isbn: string): bool =
    let val sum = isbnSum (isbnDigits isbn) 10 0
    in  case sum
          of SOME (s) => s mod 11 = 0
           | NONE     => false
    end
end
