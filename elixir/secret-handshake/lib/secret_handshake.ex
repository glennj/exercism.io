defmodule SecretHandshake do
  import Bitwise

  @actions ["wink", "double blink", "close your eyes", "jump"]

  @doc """
  Determine the actions of a secret handshake based on the binary
  representation of the given `code`.
  """
  @spec commands(code :: integer) :: list(String.t())
  def commands(code) do
    commands =
      for {action, idx} <- Enum.with_index(@actions),
          ((code >>> idx) &&& 1) == 1,
          do: action

    case (code >>> length(@actions)) &&& 1 do
      0 -> commands
      1 -> Enum.reverse(commands)
    end
  end
end
