defmodule Change do
  @doc """
    Determine the least number of coins to be given to the user such
    that the sum of the coins' value would equal the correct amount of change.
    It returns {:error, "cannot change"} if it is not possible to compute the
    right amount of coins. Otherwise returns the tuple {:ok, list_of_coins}
  """

  @spec generate(list(integer), integer) :: {:ok, list(integer)} | {:error, String.t()}
  def generate(_coins, target) when target < 0, do: {:error, "cannot change"}

  def generate(_coins, 0), do: {:ok, []}

  def generate(coins, target) do
    # `s` maps a target value to the _first_ coin to be used to make change
    s = change(coins, target)

    case Map.get(s, target) do
      nil -> {:error, "cannot change"}
      _ -> {:ok, make_change(coins, target, s, [])}
    end
  end

  # Change making algorithm from
  # http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf
  #
  # C = maps the minimum number of coins required to make
  #     change for each n from 1 to amount.
  #
  # S = the _first_ coin used to make change for amount n
  #     (actually stores the coin _index_ into the
  #     denominations array)
  #
  # This function returns the S map
  #
  defp change(coins, target) do
    do_change(coins, target, 1, %{0 => 0}, %{})
  end

  @spec do_change(
          coins :: list(integer),
          target :: integer,
          n :: pos_integer,
          c :: map,
          s :: map
        ) :: map
  defp do_change(_, target, n, _, s) when n > target, do: s

  defp do_change(coins, target, n, c, s) do
    # what's elixir "infinity"?
    {min, coin} = _inner_loop(coins, c, 0, n, {999_999_999, nil})
    c = Map.put(c, n, min)
    s = Map.put(s, n, coin)
    do_change(coins, target, n + 1, c, s)
  end

  # _inner_loop: find both the minimum number of coins required
  # and the first coin required to make change for value n.
  #
  @spec _inner_loop(
          coins :: list(integer),
          c :: map,
          i :: integer,
          n :: integer,
          {min :: integer, coin :: integer}
        ) :: {integer, integer}
  defp _inner_loop(coins, _, i, _, result) when i == length(coins), do: result

  defp _inner_loop(coins, c, i, n, {min, coin}) do
    coin_value = Enum.at(coins, i)

    # c_x: the x'th entry in C map:
    # the number of coins required to make change for value x
    c_x = Map.get(c, n - coin_value)

    if coin_value <= n and 1 + c_x < min do
      _inner_loop(coins, c, i + 1, n, {1 + c_x, i})
    else
      _inner_loop(coins, c, i + 1, n, {min, coin})
    end
  end

  # ------------------------------------------------------------
  defp make_change(_, 0, _, change), do: Enum.reverse(change)

  defp make_change(coins, n, s, change) do
    coin = Enum.at(coins, s[n])
    make_change(coins, n - coin, s, [coin | change])
  end
end
