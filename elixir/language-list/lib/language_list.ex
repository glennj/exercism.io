defmodule LanguageList do
  @exciting_language "Elixir"

  def new(), do: []

  def add(list, language), do: [language | list]

  def remove([_ | tail]), do: tail

  def first([head | _]), do: head

#  def count(list) do
#    case list do
#      [] -> 0
#      _ -> 1 + count(tl(list))
#    end
#  end

  def count([]), do: 0
  def count([_ | tail]), do: 1 + count(tail)

  def exciting_list?(list), do: @exciting_language in list
end
