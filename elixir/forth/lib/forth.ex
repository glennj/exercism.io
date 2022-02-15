defmodule Forth do
  defmodule Stack do
    use Agent

    def new() do
      {:ok, pid} = Agent.start_link(fn -> [] end)
      pid
    end

    def dump(stack), do: Agent.get(stack, & &1)

    def push(stack, number) do
      Agent.update(stack, &([number | &1]))
    end

    def pop(stack, n \\ 1) do
      values = Agent.get_and_update(stack, fn stack ->
        {Enum.take(stack, n), Enum.drop(stack, n)}
      end)

      if length(values) < n do
        raise Forth.StackUnderflow
      else
        values
      end
    end

    def add(stack) do
      #IO.inspect([:add, :before, Stack.dump(stack)])
      [a, b] = Stack.pop(stack, 2)
      push(stack, a + b)
      #IO.inspect([:add, :after, Stack.dump(stack)])
      stack
    end

    def sub(stack) do
      #IO.inspect([:sub, :before, Stack.dump(stack)])
      [a, b] = Stack.pop(stack, 2)
      push(stack, b - a)
      #IO.inspect([:sub, :after, Stack.dump(stack)])
      stack
    end

    def mul(stack) do
      [a, b] = Stack.pop(stack, 2)
      push(stack, b * a)
      stack
    end

    def div(stack) do
      [a, b] = Stack.pop(stack, 2)
      if a == 0, do: raise Forth.DivisionByZero
      push(stack, Kernel.div(b, a))
      stack
    end

    def dup(stack) do
      [a] = Stack.pop(stack, 1)
      push(stack, a)
      push(stack, a)
      stack
    end

    def drop(stack) do
      Stack.pop(stack, 1)
      stack
    end

    def swap(stack) do
      [a, b] = Stack.pop(stack, 2)
      push(stack, a)
      push(stack, b)
      stack
    end

    def over(stack) do
      [a, b] = Stack.pop(stack, 2)
      push(stack, b)
      push(stack, a)
      push(stack, b)
      stack
    end
  end

  @opaque evaluator :: any

  @doc """
  Create a new evaluator.
  """
  @spec new() :: evaluator
  def new() do
    Stack.new()
  end

  @doc """
  Evaluate an input string, updating the evaluator state.
  """
  @spec eval(evaluator, String.t()) :: evaluator
  def eval(ev, s) do
    tokens =
      Regex.scan(~r/[^[:space:]]+/, s)
      |> Enum.map(&hd/1)

    do_eval(ev, tokens)
  end

  defp do_eval(ev, []), do: ev
  defp do_eval(ev, [token | tokens]) do
    if ":" == token do
      record_macro(ev, tokens)
    else
      case token do
        "+" -> Stack.add(ev)
        "-" -> Stack.sub(ev)
        "*" -> Stack.mul(ev)
        "/" -> Stack.div(ev)
        "dup" -> Stack.dup(ev)
        "drop" -> Stack.drop(ev)
        "swap" -> Stack.swap(ev)
        "over" -> Stack.over(ev)
        _ ->
          try do
            Stack.push(ev, String.to_integer(token))
          rescue
            ArgumentError -> Forth.UnknownWord
          end
      end
      do_eval(ev, tokens)
    end
  end

  defp record_macro(ev, _tokens) do
    ev
  end

  @doc """
  Return the current stack as a string with the element on top of the stack
  being the rightmost element in the string.
  """
  @spec format_stack(evaluator) :: String.t()
  def format_stack(ev) do
    stack =
      Agent.get(ev, & &1)
      |> Enum.reverse()
      |> Enum.join(" ")
    Agent.stop(ev)
    stack
  end

  defmodule StackUnderflow do
    defexception []
    def message(_), do: "stack underflow"
  end

  defmodule InvalidWord do
    defexception word: nil
    def message(e), do: "invalid word: #{inspect(e.word)}"
  end

  defmodule UnknownWord do
    defexception word: nil
    def message(e), do: "unknown word: #{inspect(e.word)}"
  end

  defmodule DivisionByZero do
    defexception []
    def message(_), do: "division by zero"
  end
end
