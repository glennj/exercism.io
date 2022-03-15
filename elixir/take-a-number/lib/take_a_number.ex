defmodule TakeANumber do
  def start() do
    spawn(&ChildProcess.run/0)
  end
end

defmodule ChildProcess do
  def run() do
    loop(0)
  end

  defp loop(current) do
    receive do
      {:report_state, pid} ->
        send(pid, current)
        loop(current)

      {:take_a_number, pid} ->
        send(pid, current + 1)
        loop(current + 1)

      :stop ->
        Process.exit(self(), :stopped)

      _ ->
        loop(current)
    end
  end
end
