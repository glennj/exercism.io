defmodule RPNCalculatorInspection do
  def start_reliability_check(calculator, input) do
    %{
      input: input,
      pid: spawn_link(fn -> calculator.(input) end)
    }
  end

  def await_reliability_check_result(%{pid: pid, input: input}, results) do
    receive do
      {:EXIT, ^pid, :normal} -> Map.put(results, input, :ok)
      {:EXIT, ^pid, _} -> Map.put(results, input, :error)
    after
      100 -> Map.put(results, input, :timeout)
    end
  end

  def reliability_check(calculator, inputs) do
    old_flag = Process.flag(:trap_exit, true)

    checks =
      for input <- inputs do
        start_reliability_check(calculator, input)
      end

    results =
      for check <- checks, reduce: %{} do
        acc -> await_reliability_check_result(check, acc)
      end

    Process.flag(:trap_exit, old_flag)

    results
  end

  def correctness_check(calculator, inputs) do
    # could use 2 for-loops as above, or:
    inputs
    |> Enum.map(&Task.async(fn -> calculator.(&1) end))
    |> Enum.map(&Task.await(&1, 100))
  end
end
