fun stringSubstrings (len: int) (s: string): string list =
  if s = "" then []
  else if size s < len then [s]
  else (String.substring (s, 0, len)) :: (stringSubstrings len (String.extract (s, len, NONE)))

fun addSpaces (chunkSize: int) (word: string): string =
  String.concatWith " " (stringSubstrings chunkSize word)

(* pad a string `s` with zero or more of `t` until the size of `s` is at least `wid` *)
fun pad (wid: int) (t: string) (s: string): string =
  if (size s) >= wid
  then s
  else pad wid t (s ^ t)

(* remove all suffixes `t` from string `s` *)
fun trim (t: string) (s: string): string =
  if not (String.isSuffix t s)
  then s
  else trim t (String.substring (s, 0, (size s) - 1))
