use "sorting.sml";

val lc = String.map Char.toLower

val sortAscending = sortBy op<=

val hash = implode o sortAscending o explode o lc

fun anagramsFor subject candidates =
  let val subLc = lc subject
      val subHash = hash subject
      fun isAnagram word = lc word <> subLc andalso hash word = subHash
  in  List.filter isAnagram candidates
  end
