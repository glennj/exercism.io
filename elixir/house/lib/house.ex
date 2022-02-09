defmodule House do
  @universe {
    %{noun: "house",                            verb: "Jack built"},
    %{noun: "malt",                             verb: "lay in"},
    %{noun: "rat",                              verb: "ate"},
    %{noun: "cat",                              verb: "killed"},
    %{noun: "dog",                              verb: "worried"},
    %{noun: "cow with the crumpled horn",       verb: "tossed"},
    %{noun: "maiden all forlorn",               verb: "milked"},
    %{noun: "man all tattered and torn",        verb: "kissed"},
    %{noun: "priest all shaven and shorn",      verb: "married"},
    %{noun: "rooster that crowed in the morn",  verb: "woke"},
    %{noun: "farmer sowing his corn",           verb: "kept"},
    %{noun: "horse and the hound and the horn", verb: "belonged to"}
  }

  @doc """
  Return verses of the nursery rhyme 'This is the House that Jack Built'.
  """
  @spec recite(start :: integer, stop :: integer) :: String.t()
  def recite(start, stop) do
    Enum.map(start..stop, &verse/1) |> Enum.join("")
  end

  defp verse(n) do
    Enum.reduce(
      n..1,
      "This is",
      fn i, v ->
        thingy = elem(@universe, i - 1)
        "#{v} the #{thingy.noun} that #{thingy.verb}"
      end
    ) <> ".\n"
  end
end
