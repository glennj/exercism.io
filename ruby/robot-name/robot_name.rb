require 'set'

# class comment
class Robot
  attr_reader :name

  def self.forget
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

  def generate_name
    name = ''
    loop do
      name = rand_letter + rand_letter + rand_digit + rand_digit + rand_digit
      break unless @@names.include? name
    end
    @@names.add name
    name
  end

  @@letters = ('A'..'Z').to_a

  def rand_letter
    @@letters.sample
  end

  def rand_digit
    rand(10).to_s
  end
end
