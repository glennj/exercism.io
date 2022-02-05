defmodule ProteinTranslation do
  @proteins %{
    "UGU" => "Cysteine",       "UCU" => "Serine",
    "UGC" => "Cysteine",       "UCC" => "Serine",
    "UUA" => "Leucine",        "UCA" => "Serine",
    "UUG" => "Leucine",        "UCG" => "Serine",
    "AUG" => "Methionine",     "UGG" => "Tryptophan",
    "UUU" => "Phenylalanine",  "UAU" => "Tyrosine",
    "UUC" => "Phenylalanine",  "UAC" => "Tyrosine",

    "UAA" => "STOP",  "UAG" => "STOP",  "UGA" => "STOP"
  }

  @doc """
  Given a codon, return the corresponding protein
  """
  @spec of_codon(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def of_codon(codon) do
    case @proteins[codon] do
      nil -> {:error, "invalid codon"}
      p -> {:ok, p}
    end
  end

  @doc """
  Given an RNA string, return a list of proteins specified by codons, in order.
  """
  @spec of_rna(String.t()) :: {:ok, list(String.t())} | {:error, String.t()}
  def of_rna(rna), do: do_of_rna(rna, [])

  defp do_of_rna("", proteins), do: {:ok, proteins}

  defp do_of_rna(rna, proteins) do
    {codon, rna} = String.split_at(rna, 3)

    case of_codon(codon) do
      {:error, _}   -> {:error, "invalid RNA"}
      {:ok, "STOP"} -> do_of_rna("", proteins)
      {:ok, p}      -> do_of_rna(rna, proteins ++ [p])
    end
  end
end
