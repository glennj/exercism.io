defmodule RPG do
  defmodule Character do
    defstruct health: 100, mana: 0
  end

  defmodule LoafOfBread do
    defstruct []
  end

  defmodule ManaPotion do
    defstruct strength: 10
  end

  defmodule Poison do
    defstruct []
  end

  defmodule EmptyBottle do
    defstruct []
  end

  #-----------------------------------------------------------
  defprotocol Edible do
    @doc """
    Someone eats something.

    Parameters: 
    - an item to eat,
    - a character that eats it.

    Returns: a tuple of: 
    - by-product of eating,
    - the character that results from the eating.
    """
    @spec eat(any(), Character) :: {any(), Character}
    def eat(item, character)
  end

  defimpl Edible, for: LoafOfBread do
    def eat(_loaf, character) do
      {
        nil,
        Map.update!(character, :health, &(&1 + 5))
      }
    end
  end

  defimpl Edible, for: ManaPotion do
    def eat(potion, character) do
      {
        %EmptyBottle{},
        Map.update!(character, :mana, &(&1 + potion.strength))
      }
    end
  end

  defimpl Edible, for: Poison do
    def eat(_potion, character) do
      {
        %EmptyBottle{},
        %{character | :health => 0}
      }
    end
  end
end
