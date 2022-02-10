defmodule Garden do
  @plant_codes %{
    "C" => :clover,
    "G" => :grass,
    "R" => :radishes,
    "V" => :violets
  }

  @default_names [
    :alice, :bob, :charlie, :david, :eve, :fred,
    :ginny, :harriet, :ileana, :joseph, :kincaid, :larry
  ]

  @doc """
    Accepts a string representing the arrangement of cups on a windowsill and a
    list with names of students in the class. The student names list does not
    have to be in alphabetical order.

    It decodes that string into the various gardens for each student and returns
    that information in a map.
  """

  @spec info(String.t(), list) :: map
  def info(info_string, student_names \\ @default_names) do
    names = Enum.sort(student_names)

    # splits the input string into rows of chunks of 2:
    #
    #   "CCRR\nVVGG" => [
    #     [["C", "C"], ["R", "R"]],
    #     [["V", "V"], ["G", "G"]]
    #   ]
    [row1, row2] =
      info_string
      |> String.split("\n")
      |> Enum.map(fn row ->
        row
        |> String.graphemes()
        |> Enum.chunk_every(2)
      end)

    # transforms the above into the students' plots
    #   [
    #     {:clover, :clover, :violets, :violets},
    #     {:radishes, :radishes, :grass, :grass}
    #   ]
    plots =
      Enum.zip(row1, row2)
      |> Enum.map(fn {first, second} ->
        (first ++ second)
        |> Enum.map(&@plant_codes[&1])
        |> List.to_tuple()
      end)

    # maps the plots to the student names
    #   %{
    #     alice: {:clover, :clover, :violets, :violets},
    #     bob: {:radishes, :radishes, :grass, :grass}
    #   }
    named_plots =
      Enum.zip(names, plots)
      |> Enum.into(%{})

    # fill out the map for the rest of the students
    Enum.reduce(names, named_plots, &Map.put_new(&2, &1, {}))
  end
end
