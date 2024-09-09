class RotationalCipher
  constructor: (shift) ->
    @alphabet = 'abcdefghijklmnopqrstuvwxyz'
    @rotated = @alphabet[shift..] + @alphabet[0...shift]
    @alphabet += @alphabet.toUpperCase()
    @rotated += @rotated.toUpperCase()

  rotate: (text) ->
    [text...]
      .map (c) => @rotateChar c
      .join ''

  rotateChar: (char) ->
    idx = @alphabet.indexOf char
    if idx == -1 then char else @rotated[idx]

module.exports = RotationalCipher
