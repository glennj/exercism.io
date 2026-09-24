## Using the [0-1 knapsack problem][1] as specified on wikipedia
## [1]: https://en.wikipedia.org/wiki/Knapsack_problem#0-1_knapsack_problem

class Knapsack
  def initialize(max_weight)
    @max_weight = max_weight
  end

  def max_value(items)
    return 0 if items.empty?

    m = (0..items.count).map { |i| Array.new(1 + @max_weight, 0) }
    0.upto(items.count - 1) do |i|
      1.upto(@max_weight) do |j|
        m[i + 1][j] = 
          if items[i].weight > j
            m[i][j]
          else
            val = m[i][j - items[i].weight] + items[i].value
            [m[i][j], val].max
          end
      end
    end
    m[items.count][@max_weight]
  end
end
