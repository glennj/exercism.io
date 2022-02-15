defmodule RPNCalculator.Exception do
  defmodule DivisionByZeroError do
    defexception message: "division by zero occurred"
  end

  defmodule StackUnderflowError do
    defexception message: "stack underflow occurred"

    @impl true
    def exception(value) do
      ex = %__MODULE__{}
      case value do
        [] -> ex
        _ -> %{ex | :message => ex.message <> ", context: #{value}"}
      end
    end
  end

  def divide([]), do: raise StackUnderflowError, "when dividing"
  def divide([_]), do: raise StackUnderflowError, "when dividing"
  def divide([0, _]), do: raise DivisionByZeroError
  def divide([b, a]), do: div(a, b)
end
