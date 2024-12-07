use "math-utils.sml";   (* mmi, gcd *)
use "string-utils.sml"; (* addSpaces *)

local
  val m = 26    (* size of alphabet *)
  val A = Char.ord #"a"

  fun encipher (phrase: string) (a: int) (f: int -> int): string =
    if gcd (a, m) <> 1
    then raise Fail "a and m must be coprime."
    else let val cs = ( (List.map Char.toLower)
                      o (List.filter Char.isAlphaNum)
                      o explode
                      ) phrase
         fun translate c = if Char.isAlpha c
                           then Char.chr (A + f (Char.ord c - A))
                           else c
         in  implode (List.map translate cs)
         end
  
in
  fun encode (key: {a: int, b: int}, phrase: string): string =
    let fun encoder x = ((#a key) * x + (#b key)) mod m
    in  addSpaces 5 (encipher phrase (#a key) encoder)
    end

  fun decode (key: {a: int, b: int}, phrase: string): string =
    let val a' = mmi ((#a key), m)
        fun decoder y = (a' * (y - (#b key))) mod m
    in  encipher phrase (#a key) decoder
    end
end
