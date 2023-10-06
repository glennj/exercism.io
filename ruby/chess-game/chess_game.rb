module StringRanges
  refine String do
    def first_n_chars(n)
      self[...n]
    end
    def last_n_chars(n)
      self[-n..]
    end
  end
end

module Chess
  using StringRanges

  RANKS = 1..8
  FILES = 'A'..'H'

  MOVE_MESSAGE = {
    true => "%s moved to %s",
    false => "%s attempted to move to %s, but that is not a valid square"
  }
  private_constant :MOVE_MESSAGE

  def self.valid_square?(rank, file)
    RANKS.include? rank and FILES.include? file
  end

  def self.nick_name(first_name, last_name)
    (first_name.first_n_chars(2) + last_name.last_n_chars(2)).upcase
  end

  def self.move_message(first_name, last_name, square)
    is_valid = valid_square?(square[-1].to_i, square[0])
    MOVE_MESSAGE[is_valid] % [
      nick_name(first_name, last_name),
      square
    ]
  end
end
