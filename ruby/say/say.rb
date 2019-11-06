require_relative 'in_english'

# class comment
class Say
  using InEnglish

  attr_reader :in_english

  def initialize(number)
    @in_english = number.say
  end
end
