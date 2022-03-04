defmodule Markdown do
  @doc """
    Parses a given string with Markdown syntax and returns the associated HTML for that string.
  """
  @spec parse(String.t()) :: String.t()
  def parse(markdown) do
    String.split(markdown, "\n")
    |> Enum.map(&process/1)
    |> enclose_unordered_lists()
  end

  # ------------------------------------------------------------
  @re_header ~r/^ (?<head> [#]{1,6}) \s+ (?<text> .+)/x

  defp process(line) do
    # first, check if it's a header
    process_headers(line, Regex.named_captures(@re_header, line))
  end

  defp process_headers(_, captures) when is_map(captures) do
    # it's a header, calculate the tag
    tag = "h#{String.length(captures["head"])}"
    captures["text"] |> enclose_with_tag(tag)
  end

  defp process_headers(line, _) do
    # it's not a header, check for list items, else paragraph
    process_li_p(line, String.split(line, ~r/\s+/, parts: 2))
  end

  defp process_li_p(_, ["*", text]) do
    line_markup(text) |> enclose_with_tag("li")
  end

  defp process_li_p(line, _) do
    line_markup(line) |> enclose_with_tag("p")
  end

  defp enclose_with_tag(text, tag) do
    "<#{tag}>#{text}</#{tag}>"
  end

  # ------------------------------------------------------------
  @re_strong ~r/\b __ \B (.+?) \B __ \b/x
  @re_em     ~r/\b _  \B (.+?) \B _  \b/x

  defp line_markup(line) do
    line
    |> do_markup(@re_strong, "strong")
    |> do_markup(@re_em, "em")
  end

  defp do_markup(line, re, tag) do
    Regex.replace(re, line, fn _, text -> enclose_with_tag(text, tag) end)
  end

  # ------------------------------------------------------------
  @spec enclose_unordered_lists(list(String.t()), boolean, String.t()) :: String.t()
  defp enclose_unordered_lists(lines, in_list \\ false, html \\ "")
  defp enclose_unordered_lists([], false, html), do: html
  defp enclose_unordered_lists([], true, html), do: html <> "</ul>"

  defp enclose_unordered_lists([line | lines], in_list, html) do
    case {in_list, String.starts_with?(line, "<li>")} do
      {true, false} ->
        enclose_unordered_lists(lines, false, html <> "</ul>" <> line)

      {false, true} ->
        enclose_unordered_lists(lines, true, html <> "<ul>" <> line)

      _ ->
        enclose_unordered_lists(lines, in_list, html <> line)
    end
  end
end
