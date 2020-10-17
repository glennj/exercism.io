import re, strutils

let pattern = rex"""
  ^\D* 1?      \D*   # optional country code
  ([2-9]\d{2}) \D*   # area code
  ([2-9]\d{2}) \D*   # exchange
  (\d{4})      \D*$  # subscriber number
"""

proc clean*(input: string): string =
  var matches: array[3, string]
  if not input.match(pattern, matches):
    raise newException(ValueError, "invalid phone number")
  matches.join("")
