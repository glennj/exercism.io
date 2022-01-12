require 'set'

# class comment
class Robot
  attr_reader :name

  def self.forget
    # Using a class variable is suboptimal, due to "inheritence problem".
    # I was trying to figure out how to use a class instance variable, and
    # also to limit access to that variable to only Robot instances. I don't
    # think Ruby can be that locked down.
    #
    # Sticking to simplest solution here.

    @@names = Set.new
  end

  forget

  def initialize
    reset
  end

  def reset
    @name = generate_name
  end

  private

  LETTERS = ('A'..'Z').to_a
  DIGITS  = ('0'..'9').to_a

  def generate_name
    name = ''
    loop do
      name = LETTERS.sample + LETTERS.sample +
             DIGITS.sample  + DIGITS.sample  + DIGITS.sample
      break unless @@names.include? name
    end
    @@names.add name
    name
  end
end
