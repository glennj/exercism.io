List = require 'pl.List'  -- https://lunarmodules.github.io/Penlight/classes/pl.List.html

a = 'a'\byte!
toNumbers = (str)    -> [byte - a for byte in *{str\byte 1, #str}]
toString = (numbers) -> string.char table.unpack [n + a for n in *numbers]

randomLetter = -> a + math.random(0, 25)
randomKey =    -> string.char table.unpack [randomLetter! for _ = 1, 100]

-- -----------------------------------------------------------------------
class SimpleCipher
  new: (key) => 
    @_k = toNumbers (key or randomKey!)

  key: => toString @_k

  encipher: (text, dir) =>
    key = List.new @_k
    while key\len! < #text do key\extend @_k
    toString [(n + dir * key[i]) % 26 for i, n in ipairs toNumbers text]

  encode: (plaintext)  => @encipher plaintext, 1
  decode: (ciphertext) => @encipher ciphertext, -1
