defmodule Allergies do
  use Bitwise

  @allergens ~w/ eggs peanuts shellfish strawberries tomatoes chocolate pollen cats /

  @doc """
  List the allergies for which the corresponding flag bit is true.
  """
  @spec list(non_neg_integer) :: [String.t()]
  def list(flags) do
    for {allergen, idx} <- Enum.with_index(@allergens),
        (flags &&& 1 <<< idx) != 0,
        do: allergen
  end

  @doc """
  Returns whether the corresponding flag bit in 'flags' is set for the item.
  """
  @spec allergic_to?(non_neg_integer, String.t()) :: boolean
  def allergic_to?(flags, item) do
    ## we could do this, reusing the `list` function:
    #
    # item in list(flags)
    #
    ## but this is slightly more efficient:
    0 !==
      for {allergen, idx} <- Enum.with_index(@allergens),
          item == allergen,
          (flags &&& 1 <<< idx) != 0,
          reduce: 0
      do
        result -> result ||| 1 <<< idx
      end
  end
end
