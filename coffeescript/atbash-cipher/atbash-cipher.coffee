class AtbashCipher

  # dynamically set up the character mapping
  # this should only happen once, when the module is required.
  alphabet = ['abcdefghijklmnopqrstuvwxyz'...]
  reversed = alphabet.slice().reverse()
  mapping = new Map()
  for a, i in alphabet
    mapping.set a, reversed[i]
  for d in ['0123456789'...]
    mapping.set d, d

  @decode: (phrase) ->
    phrase.toLowerCase()
          .replace /[^\p{Letter}\p{Number}]/ug, ''
          .split ''
          .map (c) -> mapping.get c
          .join ''

  @encode: (phrase) -> 
    @decode phrase
      .replace /.{5}/g, "$& "
      .trimEnd()

module.exports = AtbashCipher
