defmodule MatchingBrackets do
  @doc """
  Checks that all the brackets and braces in the string are matched correctly, and nested correctly
  """
  @spec check_brackets(String.t()) :: boolean
  def check_brackets(str), do: do_check_brackets(to_charlist(str), '')

  # when no more characters, the stack must be empty
  defp do_check_brackets('', ''), do: true
  defp do_check_brackets('', _), do: false

  # add open bracket to stack
  defp do_check_brackets([c | chars], stack) when c in '({[' do
    do_check_brackets(chars, [c | stack])
  end

  # not a close bracket, ignore this character
  defp do_check_brackets([c | chars], stack) when c not in ')}]' do
    do_check_brackets(chars, stack)
  end

  # found a close bracket when the stack is empty
  defp do_check_brackets(_, ''), do: false

  # found a match
  defp do_check_brackets([c | chars], [b | stack])
    when (b == ?[ and c == ?])
      or (b == ?( and c == ?))
      or (b == ?{ and c == ?})
  do
    do_check_brackets(chars, stack)
  end

  # mismatched
  defp do_check_brackets(_, _), do: false
end
