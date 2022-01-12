# frozen_string_literal: true

# I can generate names for Robots
#
# This implementation initializes all the names up-front,
# and maintains an index into the collection of names.
#
# This greatly improves performance when the names are getting
# close to consumed.
#   - comparing against an implementation that generates a random
#     name then checks a set to see if it's been used, the
#     test suite runs this code almost 10x faster.
#
class Robot
  attr_reader :name

  # generate all names, store in class instance variable
  @names = []
  ('A'..'Z').each   do |a|
    ('A'..'Z').each do |b|
      0.upto(999)   do |c|
        @names << format('%s%s%03d', a, b, c) # rubocop:disable Style/FormatStringToken
      end
    end
  end

  def self.forget
    @names.shuffle!
    @idx = -1
  end

  forget

  def initialize
    reset
  end

  def reset
    @name = self.class.next_name
  end

  def self.next_name
    @idx += 1
    raise 'all names in use' if @idx == @names.length

    @names[@idx]
  end
end
