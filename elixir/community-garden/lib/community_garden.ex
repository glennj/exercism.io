# Use the Plot struct as it is provided
defmodule Plot do
  @enforce_keys [:plot_id, :registered_to]
  defstruct [:plot_id, :registered_to]
end

defmodule CommunityGarden do
  use Agent

  def start(), do: start([])

  def start(opts) do
    Agent.start_link(fn -> %{plots: [], next_id: 1} end, opts)
  end

  def list_registrations(pid) do
    state = Agent.get(pid, fn list -> list end)
    state.plots
  end

  def register(pid, register_to) do
    Agent.get_and_update(pid, fn state ->
      new_plot = %Plot{plot_id: state.next_id, registered_to: register_to}

      new_state =
        state
        |> Map.update!(:plots, &[new_plot | &1])
        |> Map.update!(:next_id, &(&1 + 1))

      {new_plot, new_state}
    end)
  end

  def release(pid, plot_id) do
    Agent.update(pid, fn state ->
      Map.update!(
        state,
        :plots,
        &Enum.filter(&1, fn plot -> plot.plot_id != plot_id end)
      )
    end)
  end

  def get_registration(pid, plot_id) do
    Agent.get(pid, fn state ->
      Enum.find(
        state.plots,
        {:not_found, "plot is unregistered"},
        fn plot -> plot.plot_id == plot_id end
      )
    end)
  end
end
