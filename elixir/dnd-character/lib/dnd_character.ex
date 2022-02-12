defmodule DndCharacter do
  @type t :: %__MODULE__{
          strength: pos_integer(),
          dexterity: pos_integer(),
          constitution: pos_integer(),
          intelligence: pos_integer(),
          wisdom: pos_integer(),
          charisma: pos_integer(),
          hitpoints: pos_integer()
        }

  defstruct ~w[strength dexterity constitution intelligence wisdom charisma hitpoints]a

  @spec modifier(pos_integer()) :: integer()
  def modifier(score) do
    Integer.floor_div(score - 10, 2)
  end

  @spec ability :: pos_integer()
  def ability do
    rolls =
      for _ <- 1..4 do
        Enum.random(1..6)
      end

    Enum.sum(rolls) - Enum.min(rolls)
  end

  @spec character :: t()
  def character do
    character =
      for characteristic <- Map.keys(%__MODULE__{}),
          characteristic != :__struct__,
          reduce: %__MODULE__{}
      do
        char -> %{char | characteristic => ability()}
      end

    %{character | :hitpoints => 10 + modifier(character.constitution)}
  end
end
