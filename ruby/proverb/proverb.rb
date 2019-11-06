class Proverb
  private

  attr_reader :words, :qualifier

  public

  def initialize(*words, qualifier: '')
    raise ArgumentError if words.length < 2
    @words = words
    @qualifier = qualifier
  end

  def to_s
    words
      .each_cons(2)
      .collect { |this, that| "For want of a #{this} the #{that} was lost." }
      .concat(["And all for the want of a #{(qualifier + ' ').lstrip}#{words.first}."])
      .join("\n")
  end
end
