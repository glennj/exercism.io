truncate = (str, size=5) ->
  last_byte_pos = utf8.offset(str, size) or str\len!
  utf8.char(utf8.codepoint(str, 1, last_byte_pos))

{ :truncate }
