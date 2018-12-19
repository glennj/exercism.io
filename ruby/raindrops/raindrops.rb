module Raindrops
  module_function
  def convert(n)
    sounds = []
    sounds.push 'Pling' if n % 3 == 0
    sounds.push 'Plang' if n % 5 == 0
    sounds.push 'Plong' if n % 7 == 0
    sounds.empty? ? n.to_s : sounds.join

=begin
    # Alternately
    sounds = [[3,'i'], [5,'a'], [7,'o']]
      .select {|(d, v)| n % d == 0}
      .map {|(d,v)| "Pl#{v}ng"}
      .join
    sounds.empty? ? n.to_s : sounds
=end

  end
end