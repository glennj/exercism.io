defmodule Forth do
  alias Forth.Stack, as: Stack

  @opaque evaluator :: Stack.t()

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
      Regex.scan(~r/[^[:space:][:cntrl:]]+/u, s)
      |> Enum.map(&hd/1)
      |> Enum.map(&String.downcase/1)

    do_eval(ev, tokens)
  end

  defp do_eval(ev, []), do: ev

  defp do_eval(ev, [token | tokens]) do
    cond do
      ":" == token ->
        tokens = record_macro(ev, tokens)
        do_eval(ev, tokens)

      macro = Stack.get_macro(ev, token) ->
        do_eval(ev, macro ++ tokens)

      true ->
        case token do
          "+" ->
            Stack.add(ev)

          "-" ->
            Stack.sub(ev)

          "*" ->
            Stack.mul(ev)

          "/" ->
            Stack.div(ev)

          "dup" ->
            Stack.dup(ev)

          "drop" ->
            Stack.drop(ev)

          "swap" ->
            Stack.swap(ev)

          "over" ->
            Stack.over(ev)

          _ ->
            try do
              Stack.push(ev, String.to_integer(token))
            rescue
              ArgumentError -> raise Forth.UnknownWord
            end
        end

        do_eval(ev, tokens)
    end
  end

  defp record_macro(ev, [name | tokens]) do
    case validate_macro_name(name) do
      :error -> raise Forth.InvalidWord
      :ok ->
        {remaining_tokens, macro} = extract_macro(tokens, [])
        Stack.add_macro(ev, name, macro)
        remaining_tokens
    end
  end

  defp validate_macro_name(name) do
    try do
      _i = String.to_integer(name)
      :error
    rescue
      ArgumentError -> :ok
    end
  end

  defp extract_macro([";" | ts], macro), do: {ts, Enum.reverse(macro)}
  defp extract_macro([t | ts], macro), do: extract_macro(ts, [t | macro])

  @doc """
  Return the current stack as a string with the element on top of the stack
  being the rightmost element in the string.
  """
  @spec format_stack(evaluator) :: String.t()
  def format_stack(ev) do
    Stack.to_string(ev)
  end
end
