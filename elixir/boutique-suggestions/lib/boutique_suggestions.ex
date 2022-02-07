defmodule BoutiqueSuggestions do
  def get_combinations(tops, bottoms, options \\ []) do
    # options = options ++ [maximum_price: 100.00]
    options = Keyword.put_new(options, :maximum_price, 100.0)

    for top <- tops,          # the first generator MUST be on the same line as `for`
        bottom <- bottoms,
        top.base_color != bottom.base_color,
        top.price + bottom.price <= options[:maximum_price]
    do
      {top, bottom}
    end
  end
end
