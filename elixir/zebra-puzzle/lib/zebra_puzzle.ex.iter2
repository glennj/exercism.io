defmodule ZebraPuzzle do
  use Agent
  import Permutations

  @houses [1, 3, 5, 7, 9]                         # clue 1
  @first 1
  @middle 5

  defp right_of?(a, b), do: a == b + 2
  defp next_to?(a, b), do: abs(a - b) == 2

  # ------------------------------------------------------------
  @doc """
  Determine who drinks the water
  """
  @spec drinks_water() :: atom
  def drinks_water() do
    case solve() do
      {:ok, result} -> result[:water_drinker]
      not_ok -> not_ok
    end
  end

  @doc """
  Determine who owns the zebra
  """
  @spec owns_zebra() :: atom
  def owns_zebra() do
    case solve() do
      {:ok, result} -> result[:zebra_owner]
      not_ok -> not_ok
    end
  end

  # ------------------------------------------------------------
  defp solve() do
    {:ok, pid} = Agent.start_link(fn -> [] end)

    for [red, green, ivory, yellow, blue] <- permutations(@houses),
        right_of?(green, ivory)                   # clue 6
    do
      for [english, spanish, ukranian, norwegian, japanese] <- permutations(@houses),
          english == red,                         # clue 2
          norwegian == @first,                    # clue 10
          next_to?(norwegian, blue)               # clue 15
      do
        nationalities = %{
          english => :englishman,
          spanish => :spanish,
          ukranian => :ukranian,
          norwegian => :norwegian,
          japanese => :japanese
        }

        for [coffee, tea, milk, orange_juice, water] <- permutations(@houses),
            coffee == green,                      # clue 4
            tea == ukranian,                      # clue 5
            milk == @middle                       # clue 9
        do
          for [old_gold, kools, chesterfields, lucky_strike, parliaments] <-
                permutations(@houses),
              kools == yellow,                    # clue 8
              lucky_strike == orange_juice,       # clue 13
              parliaments == japanese             # clue 14
          do
            for [dog, snails, fox, horse, zebra] <- permutations(@houses),
                dog == spanish,                   # clue 3
                snails == old_gold,               # clue 7
                next_to?(fox, chesterfields),     # clue 11
                next_to?(horse, kools)            # clue 12
            do
              Agent.update(pid, &(
                [[
                  water_drinker: nationalities[water],
                  zebra_owner: nationalities[zebra]
                ] | &1]
              ))
            end
          end
        end
      end
    end

    case Agent.get(pid, & &1) do
      [solution] -> {:ok, solution}
      [] -> {:error, :no_solution}
      _ -> {:error, :multiple_solutions}
    end
  end
end
