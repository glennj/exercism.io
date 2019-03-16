# Change making algorithm from
# http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf

module Change

  class ImpossibleCombinationError < StandardError; end
  class NegativeTargetError        < StandardError; end

  class << self

    def generate(coins, amount)
      return [] if amount.zero?
      raise NegativeTargetError if amount.negative?

      make_change(change(coins, amount), coins, amount)
    end

    private

    # This function generates two arrays:
    #
    # min_coins:
    #     maps the minimum number of coins required to make
    #     change for each n from 1 to amount.
    #
    # first_coin:
    #     the _first_ coin used to make change for amount n
    #     (actually stores the coin _index_ into the
    #     denominations array)

    def change(coins, amount)
      min_coins = [0]
      first_coin = []

      1.upto(amount).each do |n|
        min = 1.0/0.0       # Infinity
        coin = nil

        coins.each_with_index do |denom, i|
          if denom <= n then
            if 1 + min_coins[n - denom] < min then
              min = 1 + min_coins[n - denom]
              coin = i
            end
          end
        end

        min_coins[n] = min
        first_coin[n] = coin
      end

      return first_coin
    end

    # determine wo many of each coin to use
    def make_change(first_coin, coins, amount)
      raise ImpossibleCombinationError if first_coin[amount].nil?

      result = []
      while amount > 0 do
        coin = coins[first_coin[amount]]
        result << coin
        amount -= coin
      end
      result
    end
  end
end
