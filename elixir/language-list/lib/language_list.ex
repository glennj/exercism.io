defmodule LanguageList do
  def new(), do: []

  def add(list, language), do: [language | list]

  def remove([]), do: []            # not required by tests
  def remove([_ | tail]), do: tail

  def first([]), do: nil            # not required by tests
  def first([head | _]), do: head

  ## recursive but not tailcall recursive
  #def count(list) do
  #  case list do
  #    [] -> 0
  #    [_ | tail] -> 1 + count(tail)
  #  end
  #end


  def count(list), do: count(list, 0)

  defp count([], count), do: count
  defp count([_ | tail], count), do: count(tail, 1 + count)

  def exciting_list?(list), do: "Elixir" in list
end
