["fly", "spider", "bird", "cat", "dog", "goat", "cow", "horse"] as $animals
| [
    "I don't know why she swallowed the fly. Perhaps she'll die.",
    "It wriggled and jiggled and tickled inside her.",
    "How absurd to swallow a bird!",
    "Imagine that, to swallow a cat!",
    "What a hog, to swallow a dog!",
    "Just opened her throat and swallowed a goat!",
    "I don't know how she swallowed a cow!",
    "She's dead, of course!"
  ] as $catchphrase
| [
    "She swallowed the spider to catch the fly.",
    "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.",
    "She swallowed the cat to catch the bird.",
    "She swallowed the dog to catch the cat.",
    "She swallowed the goat to catch the dog.",
    "She swallowed the cow to catch the goat."
  ] as $swallow
| [ foreach range(.startVerse - 1; .endVerse) as $i (null;
      # generate verse $i
      ["I know an old lady who swallowed a \($animals[$i])."]
      + [$catchphrase[$i]]
      + if ($animals[$i] | IN("horse","fly"))
          then null
          else ($swallow[0:$i] | reverse) + [$catchphrase[0]]
        end
      | join("\n")
    )
  ]
| join("\n\n")
