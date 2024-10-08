class Game
  class BowlingError < ArgumentError; end

  private

  attr_accessor :game_score
  attr_accessor :frame
  attr_accessor :current
  attr_accessor :bonuses

  public 

  def initialize
    @game_score = 0
    @frame = 1
    @current = []
    @bonuses = []
  end

  def score
    return game_score if game_over
    raise BowlingError, 'Cannot score a game still in progress'
  end

  def roll(pins)
    raise BowlingError, 'Game is over' if game_over
    raise BowlingError, "Invalid roll: #{pins}" unless (0..10).cover? pins
    raise BowlingError, "Too many pins #{[current, pins]}" if too_many_pins pins

    add_score pins
    handle_frame pins
  end

  private

  def game_over
    frame > 10
  end

  def tenth_frame
    frame == 10
  end

  def add_score(pins)
    self.game_score = game_score + pins

    bonuses.each_index do |i|
      self.game_score = game_score + pins
      bonuses[i] -= 1
    end
    bonuses.delete_if(&:zero?)
  end

  def handle_frame(pins)
    if tenth_frame
      handle_tenth_frame pins
    else
      handle_nth_frame pins
    end
  end

  def handle_nth_frame(pins)
    if pins == 10
      self.frame = frame + 1
      bonuses << 2
    elsif current.empty?
      current << pins
    else
      bonuses << 1 if current.first + pins == 10
      self.frame = frame + 1
      self.current = []
    end
  end

  def handle_tenth_frame(pins)
    current << pins
    self.frame = frame + 1 if current.size == 3 || (current.size == 2 && current.sum < 10)
  end

  def too_many_pins(pins)
    return false if current.empty?
    return too_many_pins_tenth pins if tenth_frame
    current.first + pins > 10
  end

  def too_many_pins_tenth(pins)
    non_strike = current.reject { |roll| roll == 10 }
    return false if non_strike.empty?
    return false if non_strike.size == 2 && non_strike.sum == 10
    non_strike.first + pins > 10
  end
end
