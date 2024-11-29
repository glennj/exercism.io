local
  val a = Char.ord #"a"
  val z = Char.ord #"z"
  
  fun encipher c = 
    if Char.isAlpha c
    then Char.chr (a + z - Char.ord c)
    else c
  
  fun addSpaces chunkSize word =
    let fun chunkify chunks [] = chunks
          | chunkify chunks cs =
              if length cs <= chunkSize
              then cs :: chunks
              else let val first = List.take (cs, chunkSize)
                       val rest  = List.drop (cs, chunkSize)
                   in  chunkify (first :: chunks) rest
                   end
        val chunks = (List.rev o (List.map implode) o (chunkify []) o explode) word
    in  String.concatWith " " chunks
    end
in
  fun decode (phrase: string): string =
    let val alnum   = List.filter Char.isAlphaNum (explode phrase)
        val decoded = List.map (encipher o Char.toLower) alnum
    in  implode decoded
    end
  
  val encode = (addSpaces 5) o decode
end
