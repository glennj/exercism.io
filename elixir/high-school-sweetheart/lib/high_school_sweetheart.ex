defmodule HighSchoolSweetheart do
  def first_letter(name) do
    name |> String.trim_leading() |> String.at(0)
  end

  def initial(name) do
    String.upcase(first_letter(name)) <> "."
  end

  def initials(full_name) do
    full_name
    |> String.split()
    |> Enum.map(&initial/1)
    |> Enum.join(" ")
  end

  def pair(full_name1, full_name2) do
    # heredocs: the indentation of the _closing_ quotes is trimmed from each line
    """
         ******       ******
       **      **   **      **
     **         ** **         **
    **            *            **
    **                         **
    **     #{initials(full_name1)}  +  #{initials(full_name2)}     **
     **                       **
       **                   **
         **               **
           **           **
             **       **
               **   **
                 ***
                  *
    """
  end
end
