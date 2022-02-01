defmodule LanguageList do
  def new() do
    []
  end

  def add(list, language) do
    [language | list]
  end

  def remove([_ | tail]) do
    tail
  end

  def first([head | _]) do
    head
  end

  def count(list) do
    cond do
      list == [] -> 0
      [_ | tail] = list -> 1 + count(tail)
    end
  end

## count implemented recursively, take 2: pattern matching
#  def count([_ | tail]) do
#    1 + count(tail)
#  end
#  def count([]) do
#    0
#  end

  def exciting_list?(list) do
    "Elixir" in list
  end
end
