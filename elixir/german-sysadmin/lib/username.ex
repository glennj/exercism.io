defmodule Username do
  @spec sanitize(username :: charlist) :: charlist

  def sanitize(''), do: ''

  def sanitize([char | rest]) do
    case char do
      char when char in ?a..?z -> [char]
      ?_ -> '_'
      ?ä -> 'ae'
      ?ö -> 'oe'
      ?ü -> 'ue'
      ?ß -> 'ss'
      _  -> ''
    end ++ sanitize(rest)
  end
end
