defmodule FoodChain do
  @menagerie [
    fly: %{},
    spider: %{
      extra: " that wriggled and jiggled and tickled inside her",
      rhyme: "It wriggled and jiggled and tickled inside her."
    },
    bird: %{rhyme: "How absurd to swallow a bird!"},
    cat: %{rhyme: "Imagine that, to swallow a cat!"},
    dog: %{rhyme: "What a hog, to swallow a dog!"},
    goat: %{rhyme: "Just opened her throat and swallowed a goat!"},
    cow: %{rhyme: "I don't know how she swallowed a cow!"},
    horse: %{lethal: "She's dead, of course!"}
  ]

  @doc """
  Generate consecutive verses of the song 'I Know an Old Lady Who Swallowed a Fly'.
  """
  @spec recite(start :: integer, stop :: integer) :: String.t()
  def recite(start, stop) do
    for i <- start..stop do
      verse(i)
    end
    |> Enum.join("\n")
  end

  defp verse(n) do
    [[{start_animal, properties}] | rest] = animals(n)

    first = first_line(start_animal, properties[:rhyme])

    case properties[:lethal] do
      nil -> swallow_rest(rest, first) <> last_line()
      dead -> first <> dead <> "\n"
    end
  end

  defp animals(n) do
    @menagerie
    |> Enum.take(n)
    |> Enum.chunk_every(2, 1)
    |> Enum.reverse()
  end

  defp first_line(animal, nil), do: "I know an old lady who swallowed a #{animal}.\n"
  defp first_line(animal, rhyme), do: "#{first_line(animal, nil)}#{rhyme}\n"

  defp last_line(), do: "I don't know why she swallowed the fly. Perhaps she'll die.\n"

  defp swallow_rest([], verse), do: verse

  defp swallow_rest([pair | rest], verse) do
    [{prey, properties}, {predator, _}] = pair
    extra = properties[:extra] || ""

    swallow_rest(
      rest,
      verse <> "She swallowed the #{predator} to catch the #{prey}#{extra}.\n"
    )
  end
end
