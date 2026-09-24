# rubocop:disable Style/FrozenStringLiteralComment

## Using the [0-1 knapsack problem][1] as specified on wikipedia
## [1]: https://en.wikipedia.org/wiki/Knapsack_problem#0-1_knapsack_problem
##
class Knapsack
  def initialize(max_weight)
    @max_weight = max_weight
  end

  def max_value(items)
    values = Array.new(items.count + 1) { Array.new(@max_weight + 1, 0) }
    items.each_with_index do |item, i|
      1.upto(@max_weight) do |wt|
        val = [values[i][wt]]
        val << (values[i][wt - item.weight] + item.value) if item.weight <= wt
        values[i + 1][wt] = val.max
      end
    end
    values[items.count][@max_weight]
  end
end

# rubocop:enable Style/FrozenStringLiteralComment
