m = 26   -- size of english alphabet
a = string.byte 'a'

to_num  = (char) -> string.byte(char) - a 
to_char = (num)  -> string.char(num + a)

mmi = (a, m) ->
  for i = 1, m
    if (a * i) % m == 1
      return i

gcd = (a, b) -> if b == 0 then a else gcd b, a%b

spaced = (text) -> text\gsub('.....', '%0 ')\gsub('%s+$', '')

encrypt = (text, fn) -> text\lower!\gsub('%W', '')\gsub '%a', fn

{
  encode: (phrase, key) ->
    assert gcd(key.a, m) == 1, 'a and m must be coprime.'
    spaced encrypt phrase, (char) -> to_char (key.a * to_num(char) + key.b) % m

  decode: (phrase, key) ->
    assert gcd(key.a, m) == 1, 'a and m must be coprime.'
    a_inv = mmi key.a, m
    encrypt phrase, (char) -> to_char (a_inv * (to_num(char) - key.b)) % m
}
