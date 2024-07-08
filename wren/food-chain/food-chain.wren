class FoodChain {
  static recite(verse) { new().verse(verse) }

  static recite(startVerse, endVerse) {
    var chain = new()
    var lines = (startVerse..endVerse).reduce([]) {|lines, n|
      lines.add("")
      lines.addAll(chain.verse(n))
      return lines
    }
    return lines.skip(1).toList
  }

  construct new() {
    _animals = [
      Fly.new(), Spider.new(), Bird.new(), Cat.new(),
      Dog.new(), Goat.new(), Cow.new(), Horse.new()
    ]
  }

  verse(n) {
    var i = n - 1
    var lines = []
    lines.add("I know an old lady who swallowed a %(_animals[i].name).")
    if (n > 1) {
      lines.add(_animals[i].catchPhrase)
      if (_animals[i].isKiller) {
        return lines
      }
      for (j in i..1) {
        lines.add("She swallowed the %(_animals[j].name) to catch the %(_animals[j-1].prey).")
      }
    }
    lines.add(_animals[0].catchPhrase)
    return lines
  }
}

// ------------------------------------------------------------
class Animal {
  construct new(name) { _name = name }
  name { _name }
  prey { _name }
  isKiller { false }
  catchPhrase { Fiber.abort("subclass responsibility") }
}

class Fly is Animal {
  construct new() { super("fly") }
  catchPhrase { "I don't know why she swallowed the fly. Perhaps she'll die." }
}

class Spider is Animal {
  construct new() { super("spider") }
  catchPhrase { "It wriggled and jiggled and tickled inside her." }
  prey { "spider that %(catchPhrase[3..-2])" }
}

class Bird is Animal {
  construct new() { super("bird") }
  catchPhrase { "How absurd to swallow a bird!" }
}

class Cat is Animal {
  construct new() { super("cat") }
  catchPhrase { "Imagine that, to swallow a cat!" }
}

class Dog is Animal {
  construct new() { super("dog") }
  catchPhrase { "What a hog, to swallow a dog!" }
}

class Goat is Animal {
  construct new() { super("goat") }
  catchPhrase { "Just opened her throat and swallowed a goat!" }
}

class Cow is Animal {
  construct new() { super("cow") }
  catchPhrase { "I don't know how she swallowed a cow!" }
}

class Horse is Animal {
  construct new() { super("horse") }
  catchPhrase { "She's dead, of course!" }
  isKiller { true }
}
