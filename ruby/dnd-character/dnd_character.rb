class DndCharacter
  def self.modifier(points)
    # silly OO arithmetic
    points.-(10).div(2)
  end

  attr_reader :strength, :dexterity, :constitution, :intelligence, :wisdom, :charisma, :hitpoints

  def initialize
    rand = Random.new

    ability = Proc.new do
      dice = 4.times.collect { rand.rand(1..6) }
      dice.sum - dice.min
    end

    @strength = ability.call
    @dexterity = ability.call
    @constitution = ability.call
    @intelligence = ability.call
    @wisdom = ability.call
    @charisma = ability.call

    @hitpoints = self.class.modifier(@constitution) + 10
  end
end
