class SecretHandshake
  ACTIONS = [
    [0b0001, 'wink'],
    [0b0010, 'double blink'],
    [0b0100, 'close your eyes'],
    [0b1000, 'jump']
  ].freeze

  REVERSE = 0b10000

  attr_reader :commands

  def initialize(input)
    code = input.to_i
    @commands = ACTIONS.each_with_object([]) do |(num, act), cmds|
      cmds << act if (code & num).nonzero?
    end
    @commands.reverse! if (code & REVERSE).nonzero?
  end
end
