class House
  def self.recite
    new.song
  end

  ACTORS = [
    { who: 'house that Jack built.' },
    { who: 'malt', what: 'lay in' },
    { who: 'rat', what: 'ate' },
    { who: 'cat', what: 'killed' },
    { who: 'dog', what: 'worried' },
    { who: 'cow with the crumpled horn', what: 'tossed' },
    { who: 'maiden all forlorn', what: 'milked' },
    { who: 'man all tattered and torn', what: 'kissed' },
    { who: 'priest all shaven and shorn', what: 'married' },
    { who: 'rooster that crowed in the morn', what: 'woke' },
    { who: 'farmer sowing his corn', what: 'kept' },
    { who: 'horse and the hound and the horn', what: 'belonged to' }
  ].freeze

  def initialize
    @song = Array.new(ACTORS.length) { |i| verse(i) }.join("\n")
  end
  attr_reader :song

  def verse(n)
    verse = "This is the #{ACTORS[n][:who]}\n"
    n.downto(1) do |i|
      verse << "that #{ACTORS[i][:what]} the #{ACTORS[i - 1][:who]}\n"
    end
    verse
  end
end
