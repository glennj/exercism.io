class FoodChain
  Animal = Struct.new(:name, :tag, :that, :apexPredator?, keyword_init: true)

  CHAIN = [
    nil,
    Animal.new(name: 'fly'),
    Animal.new(name: 'spider',
               that: ' that wriggled and jiggled and tickled inside her',
               tag: "It wriggled and jiggled and tickled inside her.\n"),
    Animal.new(name: 'bird',   tag: "How absurd to swallow a bird!\n"),
    Animal.new(name: 'cat',    tag: "Imagine that, to swallow a cat!\n"),
    Animal.new(name: 'dog',    tag: "What a hog, to swallow a dog!\n"),
    Animal.new(name: 'goat',   tag: "Just opened her throat and swallowed a goat!\n"),
    Animal.new(name: 'cow',    tag: "I don't know how she swallowed a cow!\n"),
    Animal.new(name: 'horse',  tag: "She's dead, of course!\n", apexPredator?: true)
  ].freeze

  def self.song
    new.song
  end

  def initialize
    @song = CHAIN.length.times.map { |i| verse(i) }.join("\n").lstrip
  end
  attr_reader :song

  private

  def verse(n)
    return if n.zero?
    animal = CHAIN[n]
    verse = i_know(animal)
    unless animal.apexPredator?
      n.downto(2) { |i| verse << hunt(CHAIN[i], CHAIN[i - 1]) }
      verse << i_dont_know
    end
    verse
  end

  def i_know(animal)
    "I know an old lady who swallowed a #{animal.name}.\n#{animal.tag || ''}"
  end

  def i_dont_know
    "I don't know why she swallowed the fly. Perhaps she'll die.\n"
  end

  def hunt(predator, prey)
    "She swallowed the #{predator.name} to catch the #{prey.name}#{prey.that || ''}.\n"
  end
end
