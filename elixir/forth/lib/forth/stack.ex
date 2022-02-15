defmodule Forth.Stack do
  @moduledoc """
  This module does the work of a Forth stack.
  It can do arithmetic and stack operations (drop, dup, etc).
  It also stores the recorded macros.
  """

  use Agent

  defstruct stack: [], macros: %{}

  @type t :: pid

  def new() do
    {:ok, pid} = Agent.start_link(fn -> %__MODULE__{} end)
    pid
  end

  def to_string(stack) do
    Agent.get(stack, & &1.stack)
    |> Enum.reverse()
    |> Enum.join(" ")
  end

  def push(stack, number) do
    Agent.update(
      stack,
      fn state -> Map.update!(state, :stack, &[number | &1]) end
    )
  end

  def pop(stack, n \\ 1) do
    values =
      Agent.get_and_update(
        stack,
        fn state ->
          Map.get_and_update(state, :stack, fn st ->
            {Enum.take(st, n), Enum.drop(st, n)}
          end)
        end
      )

    if length(values) < n do
      raise Forth.StackUnderflow
    else
      values
    end
  end

  def add(stack) do
    [a, b] = pop(stack, 2)
    push(stack, a + b)
    stack
  end

  def sub(stack) do
    [a, b] = pop(stack, 2)
    push(stack, b - a)
    stack
  end

  def mul(stack) do
    [a, b] = pop(stack, 2)
    push(stack, b * a)
    stack
  end

  def div(stack) do
    [a, b] = pop(stack, 2)
    if a == 0, do: raise(Forth.DivisionByZero)
    push(stack, Kernel.div(b, a))
    stack
  end

  def dup(stack) do
    [a] = pop(stack, 1)
    push(stack, a)
    push(stack, a)
    stack
  end

  def drop(stack) do
    pop(stack, 1)
    stack
  end

  def swap(stack) do
    [a, b] = pop(stack, 2)
    push(stack, a)
    push(stack, b)
    stack
  end

  def over(stack) do
    [a, b] = pop(stack, 2)
    push(stack, b)
    push(stack, a)
    push(stack, b)
    stack
  end

  def get_macro(stack, name) do
    Agent.get(stack, & &1.macros[name])
  end

  def add_macro(stack, name, tokens) do
    Agent.update(stack, fn state ->
      macro =
        for t <- tokens do
          Map.get(state.macros, t, t)
        end
        |> List.flatten()

      macros = Map.put(state.macros, name, macro)
      %{state | :macros => macros}
    end)

    stack
  end
end
