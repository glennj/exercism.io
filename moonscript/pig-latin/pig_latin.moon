vowel_patterns = {'^[aeiou]', '^xr', '^yt'}

two_capture_patterns = {
  '^([^aeiou]*qu)(.+)',
  '^([^aeiou]+)(y.*)',
  '^([^aeiou]+)(.*)',
}

translate_word = (word) ->
  for patt in *vowel_patterns
    return word .. 'ay' if word\match patt

  for patt in *two_capture_patterns
    m = {word\match patt}
    if #m > 0 then return m[2] .. m[1] .. 'ay'

  error "untranslatable: #{word}"

{
  translate: (phrase) ->
    table.concat [translate_word word for word in phrase\gmatch '%S+'], ' '
}
