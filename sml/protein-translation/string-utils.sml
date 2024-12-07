fun stringSubstrings (len: int) (s: string): string list =
  if s = "" then []
  else if size s < len then [s]
  else (String.substring (s, 0, len)) :: (stringSubstrings len (String.extract (s, len, NONE)))

fun addSpaces (chunkSize: int) (word: string): string =
  String.concatWith " " (stringSubstrings chunkSize word)
