defmodule Team do
  defstruct [:name, mp: 0, w: 0, d: 0, l: 0, pts: 0]

  def new(name) do
    %Team{name: name}
  end

  def win(team), do: result(team, :w, 3)
  def draw(team), do: result(team, :d, 1)
  def lose(team), do: result(team, :l, 0)

  defp result(team, result, points) do
    %{team |
      result => Map.get(team, result) + 1,
      :mp => team.mp + 1,
      :pts => team.pts + points
    }
  end

  def data(team) do
    [team.name, team.mp, team.w, team.d, team.l, team.pts]
  end
end

defmodule Tournament do
  @header "Team                           | MP |  W |  D |  L |  P"
  @format "~-30s | ~2w | ~2w | ~2w | ~2w | ~2w"

  @spec tally(input :: list(String.t())) :: String.t()
  def tally(input) do
    input
    |> process_input()
    |> sort()
    |> format_result()
  end

  defp process_input(input) do
    input
    |> Enum.map(&String.split(&1, ";", parts: 3))
    |> Enum.filter(&(length(&1) == 3))
    |> Enum.reduce(
      %{},
      fn [name1, name2, result], teams ->
        team1 = Map.get(teams, name1, Team.new(name1))
        team2 = Map.get(teams, name2, Team.new(name2))
        register_result(teams, team1, team2, result)
      end
    )
    |> Map.values()
  end

  defp register_result(teams, a, b, "win") do
    teams
    |> Map.put(a.name, Team.win(a))
    |> Map.put(b.name, Team.lose(b))
  end

  defp register_result(teams, a, b, "loss") do
    teams
    |> Map.put(a.name, Team.lose(a))
    |> Map.put(b.name, Team.win(b))
  end

  defp register_result(teams, a, b, "draw") do
    teams
    |> Map.put(a.name, Team.draw(a))
    |> Map.put(b.name, Team.draw(b))
  end

  # ignore any other result
  defp register_result(teams, _, _, _), do: teams

  # note to self: for stable sorts, ensure sort func returns true
  # when values are equal.
  defp sort(teams) do
    teams
    |> Enum.sort(&(&1.name <= &2.name))
    |> Enum.sort(&(&1.pts >= &2.pts))
  end

  defp format_result(teams) do
    rows = teams |> Enum.map(&Team.data/1) |> Enum.map(&format_line/1)
    [@header | rows] |> Enum.join("\n")
  end

  defp format_line(data), do: :io_lib.format(@format, data) |> to_string()
end

