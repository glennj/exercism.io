# rubocop:disable Style/CommentedKeyword, Style/Documentation, Style/OneClassPerFile, Metrics
# frozen_string_literal: true

require 'singleton'

module HousePredicates
  refine Integer do
    def right_of?(other)
      self + 1 == other
    end

    def next_to?(other)
      right_of?(other) || other.right_of?(self)
    end
  end
end

class ZebraPuzzle
  include Singleton

  using HousePredicates

  def self.water_drinker
    instance.nationality_of(:water)
  end

  def self.zebra_owner
    instance.nationality_of(:zebra)
  end

  # provide the flexibility to answer all the other questions
  def nationality_of(item)
    @nationality[@directory[item]]
  end

  private

  # The directory hash will hold the answers for questions like
  # "what house has the color yellow?
  # Given that house, the nationality array answers
  # "what is the nationality of the person in house X"

  attr_reader :directory, :nationality

  HOUSES = [1, 2, 3, 4, 5].freeze                       # clue 1
  FIRST = 1
  MIDDLE = 3

  private_constant :HOUSES, :FIRST, :MIDDLE

  def initialize
    @directory = {}
    @nationality = []

    # 1. colours
    HOUSES.permutation(HOUSES.size) do |(red, green, ivory, yellow, blue)|
      next unless green.right_of?(ivory)                # clue 6

      # 2. nationality
      HOUSES.permutation(HOUSES.size) do |(gb, es, ua, no, jp)|
        next unless gb == red                           # clue 2
        next unless no == FIRST                         # clue 10
        next unless no.next_to?(blue)                   # clue 15

        # 3. beverages
        HOUSES.permutation(HOUSES.size) do |(coffee, tea, milk, orange_juice, water)|
          next unless coffee == green                   # clue 4
          next unless tea == ua                         # clue 5
          next unless milk == MIDDLE                    # clue 9

          # 4. cigarettes
          HOUSES.permutation(HOUSES.size) do |(old_gold, kools, chesterfields, lucky_strikes, parliaments)|
            next unless kools == yellow                 # clue 8
            next unless lucky_strikes == orange_juice   # clue 13
            next unless parliaments == jp               # clue 14

            # 5. pets
            HOUSES.permutation(HOUSES.size) do |(dog, snails, fox, horse, zebra)|
              next unless dog == es                     # clue 3
              next unless snails == old_gold            # clue 7
              next unless fox.next_to?(chesterfields)   # clue 11
              next unless horse.next_to?(kools)         # clue 12

              # save the house number for each attribute
              @directory = {
                red:, green:, ivory:, yellow:, blue:,
                english: gb, spanish: es, ukranian: ua, norwegian: no, japanese: jp,
                coffee:, tea:, milk:, orange_juice:, water:,
                old_gold:, kools:, chesterfields:, lucky_strikes:, parliaments:,
                dog:, snails:, fox:, horse:, zebra:
              }

              # save the nationality for each house number
              @nationality[gb] = 'English'
              @nationality[es] = 'Spanish'
              @nationality[ua] = 'Ukranian'
              @nationality[no] = 'Norwegian'
              @nationality[jp] = 'Japanese'

              # rubocop:disable-next Lint/NonLocalExitFromIterator
              return
            end # pets
          end # cigarettes
        end # beverages
      end # nationality
    end # colours
  end
end

# rubocop:enable Style/CommentedKeyword, Style/Documentation, Style/OneClassPerFile, Metrics
