num = {
  [0]: "No", "One", "Two", "Three", "Four",
  "Five", "Six", "Seven", "Eight", "Nine", "Ten"
}

bottle = (n) ->
  "#{num[n]} green bottle#{n == 1 and "" or "s"}"

verse = (n) ->
  table.concat {
    "#{bottle n} hanging on the wall,",
    "#{bottle n} hanging on the wall,",
    'And if one green bottle should accidentally fall,'
    "There'll be #{(bottle n - 1)\lower!} hanging on the wall."
  }, "\n"

{
  recite: (startVerse, numToTake) ->
    table.concat [verse v for v = startVerse, startVerse - numToTake + 1, -1], "\n\n"
}
