defmodule SecretHandshake do
  import Bitwise

  @type strlist :: list(String.t())

  @actions ["wink", "double blink", "close your eyes", "jump"]

  @doc """
  Determine the actions of a secret handshake based on the binary
  representation of the given `code`.
  """
  @spec commands(code :: integer) :: strlist
  def commands(code) do
    commands =
      for {action, idx} <- Enum.with_index(@actions),
          (code &&& 1 <<< idx) != 0,
          do: action

    if (code &&& 1 <<< length(@actions)) == 0 do
      commands
    else
      Enum.reverse(commands)
    end
  end
end
