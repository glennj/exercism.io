defmodule ScaleGenerator do
  @pitches %{
    sharps: ~w/A A# B C C# D D# E F F# G G#/,
    flats: ~w/A Bb B C Db D Eb E F Gb G Ab/
  }

  @flats ~w/F Bb Eb Ab Db Gb d g c f bb eb/

  @steps %{"m" => 1, "M" => 2, "A" => 3}

  @spec step(scale :: list(String.t()), tonic :: String.t(), step :: String.t()) ::
          list(String.t())
  def step(scale, tonic, step) do
    Enum.at(rotate(scale, tonic), @steps[step])
  end

  defp rotate(scale, tonic) do
    idx = Enum.find_index(scale, &(&1 == tonic))
    Enum.drop(scale, idx) ++ Enum.take(scale, idx) ++ [tonic]
  end

  @spec chromatic_scale(tonic :: String.t()) :: list(String.t())
  def chromatic_scale(tonic \\ "C") do
    rotate(@pitches.sharps, String.capitalize(tonic))
  end

  @spec flat_chromatic_scale(tonic :: String.t()) :: list(String.t())
  def flat_chromatic_scale(tonic \\ "C") do
    rotate(@pitches.flats, String.capitalize(tonic))
  end

  @spec find_chromatic_scale(tonic :: String.t()) :: list(String.t())
  def find_chromatic_scale(tonic) when tonic in @flats, do: flat_chromatic_scale(tonic)
  def find_chromatic_scale(tonic), do: chromatic_scale(tonic)

  @spec scale(tonic :: String.t(), pattern :: String.t()) :: list(String.t())
  def scale(tonic, pattern) do
    pitches = find_chromatic_scale(tonic)
    tonic = String.capitalize(tonic)
    [tonic | do_scale(pitches, tonic, String.graphemes(pattern), [])]
  end

  defp do_scale(_, _, [], scale), do: Enum.reverse(scale)

  defp do_scale(pitches, tonic, [step | steps], scale) do
    note = step(pitches, tonic, step)
    do_scale(rotate(pitches, tonic), note, steps, [note | scale])
  end
end
