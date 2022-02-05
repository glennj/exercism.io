defmodule TopSecret do
  @spec to_ast(String.t()) :: Code.binding()
  def to_ast(string) do
    Code.string_to_quoted!(string)
  end

  @spec decode_secret_message_part(Code.binding(), [String.t()]) :: {Code.binding(), [String.t()]}
  def decode_secret_message_part({op, _, _} = ast, acc) when op in [:def, :defp] do
    {op, _, args} = get_signature(ast)
    arity = if is_list(args), do: length(args), else: 0
    fname = op |> to_string() |> String.slice(0, arity)
    {ast, [fname | acc]}
  end
  def decode_secret_message_part(ast, acc), do: {ast, acc}

  @spec get_signature(Code.binding()) :: Code.binding()
  defp get_signature({op, _, args}) when op in [:def, :defp, :when] do
    args |> hd() |> get_signature()
  end
  defp get_signature(ast), do: ast

  @spec decode_secret_message(String.t()) :: String.t()
  def decode_secret_message(string) do
    ast = to_ast(string)

    #Macro.prewalk(to_ast(string), [], fn (ast, acc) ->
    #  decode_secret_message_part(ast, acc)
    #end)
    {_, msg_parts} = Macro.prewalk(ast, [], &decode_secret_message_part/2)

    msg_parts |> Enum.reverse() |> Enum.join("")
  end

end
