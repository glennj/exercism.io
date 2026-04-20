food = {'fly', 'spider', 'bird', 'cat', 'dog', 'goat', 'cow', 'horse'}

wriggled = 'wriggled and jiggled and tickled inside her'

catchphrase =
  fly: "I don't know why she swallowed the fly. Perhaps she'll die.",
  spider: "It #{wriggled}.",
  bird: 'How absurd to swallow a bird!',
  cat: 'Imagine that, to swallow a cat!',
  dog: 'What a hog, to swallow a dog!',
  goat: 'Just opened her throat and swallowed a goat!'
  cow: "I don't know how she swallowed a cow!",
  horse: "She's dead, of course!"

previous = (n) -> food[n-1] .. (food[n-1] == 'spider' and " that #{wriggled}" or '')

verse = (n) ->
  v = "I know an old lady who swallowed a #{food[n]}."
  v ..= '\n' .. catchphrase[food[n]]
  return v if n == 1 or n == 8
  v ..= '\n' .. "She swallowed the #{food[i]} to catch the #{previous i}." for i = n, 2, -1
  v .. '\n' .. catchphrase.fly

recite = (startVerse, endVerse) ->
  table.concat [verse i for i = startVerse, endVerse], '\n\n'

{:recite}
