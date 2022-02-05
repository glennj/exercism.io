defmodule RnaTranscription do
  @map %{
    ?A => ?U,
    ?C => ?G,
    ?G => ?C,
    ?T => ?A
  }

  @doc """
  Transcribes a character list representing DNA nucleotides to RNA
  """
  @spec to_rna([char]) :: [char]
  def to_rna(dna), do: do_transcribe(dna, '')

  @spec do_transcribe([char], [char]) :: [char]
  defp do_transcribe([], rna), do: Enum.reverse(rna)

  defp do_transcribe([d | dna], rna) do
    do_transcribe(dna, [@map[d] | rna])
  end
end
