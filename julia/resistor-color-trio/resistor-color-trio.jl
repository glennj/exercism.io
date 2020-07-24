module Resistor
  Value = Dict(
    "black" => 0, "brown" => 1, "red" => 2, "orange" => 3, "yellow" => 4,
    "green" => 5, "blue" => 6, "violet" => 7, "grey" => 8, "white" => 9
  )
  Unit = "ohms"
  Prefixes = ["", "kilo", "mega", "giga"]

  function value(colours)
    @assert length(colours) ≥ 3 "must pass at least 3 colours"
    @assert all(c -> haskey(Value, c), colours) "not all colours are valid"

    first, second, third = [Value[c] for c ∈ colours]
    (10 * first + second) * 10 ^ third
  end

  function to_string(value)
    idx = 1
    while value > 0 && mod(value, 1000) == 0
      value ÷= 1000
      idx += 1
    end
    string(value, " ", Prefixes[idx], Unit)
  end
end


function label(colors)
  Resistor.to_string(Resistor.value(colors))
end

