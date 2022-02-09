defmodule School do
  @moduledoc """
  Simulate students in a school.

  Each student is in a grade.
  """

  @type school :: any()

  @doc """
  Create a new, empty school.
  """
  @spec new() :: school
  def new() do
    %{}   # maintain a directory: map(name :: String.t => grade :: int)
  end

  @doc """
  Add a student to a particular grade in school.
  """
  @spec add(school, String.t(), integer) :: {:ok | :error, school}
  def add(school, name, grade) do
    case school[name] do
      nil -> {:ok, Map.put(school, name, grade)}
      _ -> {:error, school}
    end
  end

  @doc """
  Return the names of the students in a particular grade, sorted alphabetically.
  """
  @spec grade(school, integer) :: [String.t()]
  def grade(school, grade) do
    school
    |> Enum.filter(fn ({_, g}) -> g == grade end)
    |> Enum.map(&elem(&1, 0))
    |> Enum.sort()

  end

  @doc """
  Return the names of all the students in the school sorted by grade and name.
  """
  @spec roster(school) :: [String.t()]
  def roster(school) do
    school
    |> Enum.sort(
      fn ({s1, g1}, {s2, g2}) ->
        g1 < g2 or (g1 == g2 and s1 < s2)
      end
    )
    |> Enum.map(&elem(&1, 0))
  end
end
