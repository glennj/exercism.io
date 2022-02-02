defmodule HighScore do
  @initial_score 0

  def new(), do: %{}

  def add_player(scores, name, score \\ @initial_score), do: Map.put(scores, name, score)

  def remove_player(scores, name), do: Map.delete(scores, name)

  ## somewhat over-complicated
  #def reset_score(scores, name), do: reset_score(scores, name, Map.has_key?(scores, name))
  #defp reset_score(scores, name, true), do: %{scores | name => @initial_score}
  #defp reset_score(scores, name, false) do
  #  add_player(scores, name)
  #  |> reset_score(name, true)
  #end

  def reset_score(scores, name) do
    Map.update(scores, name, @initial_score, fn _ -> @initial_score end)
  end

  ## somewhat over-complicated
  #def update_score(scores, name, score), do: update_score(scores, name, score, Map.has_key?(scores, name))
  #defp update_score(scores, name, score, true), do: %{scores | name => scores[name] + score}
  #defp update_score(scores, name, score, false) do
  #  add_player(scores, name)
  #  |> update_score(name, score, true)
  #end

  def update_score(scores, name, score) do
    Map.update(scores, name, score, &(&1 + score))
  end

  def get_players(scores), do: Map.keys(scores)
end
