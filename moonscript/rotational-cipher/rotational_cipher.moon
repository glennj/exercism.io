{
  rotate: (text, shift_key) ->

    rotate_letter = (char, base) ->
      string.char (char\byte! - base + shift_key) % 26 + base

    rotate_upper = (char) -> rotate_letter char, 65 -- 'A'
    rotate_lower = (char) -> rotate_letter char, 97 -- 'a'

    text\gsub('%u', rotate_upper)\gsub('%l', rotate_lower)
}
