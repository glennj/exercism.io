Critters = [
  {name: "fly", tag: "I don't know why she swallowed the fly. Perhaps she'll die."},
  {name: "spider", tag: "It wriggled and jiggled and tickled inside her.", action: " that wriggled and jiggled and tickled inside her"},
  {name: "bird", tag: "How absurd to swallow a bird!"},
  {name: "cat", tag: "Imagine that, to swallow a cat!"},
  {name: "dog", tag: "What a hog, to swallow a dog!"},
  {name: "goat", tag: "Just opened her throat and swallowed a goat!"},
  {name: "cow", tag: "I don't know how she swallowed a cow!"},
  {name: "horse", tag: "She's dead, of course!"},
]


class FoodChain
  @recite: (startVerse, endVerse) ->
    (@verse i for i in [startVerse .. endVerse]).join("\n\n")

  @verse: (n) ->
    animal = Critters[n - 1]
    lines = [
      "I know an old lady who swallowed a #{animal.name}.",
      animal.tag
    ]

    if animal.name isnt "horse"
      for idx in [n - 1 ... 0]
        predator = Critters[idx]
        prey = Critters[idx - 1]
        lines.push "She swallowed the #{predator.name} to catch the #{prey.name}#{prey.action ? ""}."

      if animal.name isnt "fly"
        lines.push Critters[0].tag

    return lines.join("\n")


module.exports = FoodChain
