defmodule Username do

  @substitutions %{?ä => 'ae', ?ö => 'oe', ?ü => 'ue', ?ß => 'ss'}
  @acceptable 'abcdefghijklmnopqrstuvwxyz_äöüß'

  @spec sanitize(username :: charlist) :: charlist
  def sanitize(username) do
    for char <- username, char in @acceptable do
      Map.get(@substitutions, char, [char])
    end
    |> Enum.concat()
  end

  ## recursively:
  #
  #  def sanitize(username), do: do_sanitize(username, '')
  #
  #  defp do_sanitize('', sanitized), do: Enum.reverse(sanitized)
  #
  #  defp do_sanitize([char | username], sanitized) do
  #    do_sanitize(
  #      username,
  #      case char do
  #        ?ä -> [?e, ?a | sanitized]  # ä becomes ae
  #        ?ö -> [?e, ?o | sanitized]  # ö becomes oe
  #        ?ü -> [?e, ?u | sanitized]  # ü becomes ue
  #        ?ß -> [?s, ?s | sanitized]  # ß becomes ss
  #        c when c in ?a..?z or c == ?_ -> [c | sanitized]
  #        _ -> sanitized
  #      end
  #    )
  #  end
end
