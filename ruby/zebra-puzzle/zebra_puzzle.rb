require 'singleton'

class ZebraPuzzle
  include Singleton

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

  HOUSES = [1, 2, 3, 4, 5]                                  # clue 1
  FIRST = 1
  MIDDLE = 3

  def initialize
    @directory = {}
    @nationality = []

    # 1. colours
    HOUSES.permutation(HOUSES.size) do |(red, green, ivory, yellow, blue)|
      if  right_of(green, ivory)                            # clue 6

        # 2. nationality
        HOUSES.permutation(HOUSES.size) do |(gb, es, ua, no, jp)|
          if  gb == red         &&                          # clue 2
              no == FIRST       &&                          # clue 10
              next_to(no, blue)                             # clue 15

            # 3. beverages
            HOUSES.permutation(HOUSES.size) do |(coffee, tea, milk, oj, water)|
              if  coffee == green  &&                       # clue 4
                  tea == ua        &&                       # clue 5
                  milk == MIDDLE                            # clue 9

                # 4. cigarettes
                HOUSES.permutation(HOUSES.size) do |(oldgold, kools, chester, lucky, parl)|
                  if  kools == yellow  &&                   # clue 8
                      lucky == oj      &&                   # clue 13
                      parl == jp                            # clue 14

                    # 5. pets
                    HOUSES.permutation(HOUSES.size) do |(dog, snail, fox, horse, zebra)|

                      if  dog == es              &&         # clue 3
                          snail == oldgold       &&         # clue 7
                          next_to(fox, chester)  &&         # clue 11
                          next_to(horse, kools)             # clue 12

                        @directory = {
                          red: red, green: green, ivory: ivory, yellow: yellow, blue: blue,
                          english: gb, spanish: es, ukranian: ua, norwegian: no, japanese: jp,
                          coffee: coffee, tea: tea, milk: milk, orange_juice: oj, water: water,
                          old_gold: oldgold, kools: kools, chesterfields: chester, lucky_strikes: lucky, parliaments: parl,
                          dog: dog, snails: snail, fox: fox, horse: horse, zebra: zebra,
                        }
                        @nationality[gb] = "English"
                        @nationality[es] = "Spanish"
                        @nationality[ua] = "Ukranian"
                        @nationality[no] = "Norwegian"
                        @nationality[jp] = "Japanese"

                        return
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
  end

  def right_of(a, b)
    a + 1 == b
  end

  def next_to(a, b)
    right_of(a, b) || right_of(b, a)
  end
end
