defmodule Dominoes do
  @type domino :: {1..6, 1..6}

  @doc """
  chain?/1 takes a list of domino stones and returns boolean indicating if it's
  possible to make a full chain
  """
  @spec chain?(dominoes :: [domino]) :: boolean
  def chain?([]), do: true
  def chain?([{a, b}]), do: a == b

  def chain?(dominoes) do
    start_with(dominoes, [])
  end

  defp start_with([], _), do: false
  defp start_with([{a, b} | rest], tried) do
    chain = build_chain([{a, b}], rest ++ tried)
    chain =
      if is_nil(chain),
        do: build_chain([{b, a}], rest ++ tried),
        else: chain

    if is_nil(chain) do
      start_with(rest, [{a, b} | tried])
    else
      true
    end
  end

  defp build_chain(chain, []) do
    head = elem(hd(chain), 0)
    tail = elem(List.last(chain), 1)

    if head == tail, do: chain, else: nil
  end

  defp build_chain(chain, rest) do
    case build_chain_with(chain

    nil
  end
end
