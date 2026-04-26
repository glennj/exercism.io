re = require 're'

map = {
  a: 'z', b: 'y', c: 'x', d: 'w', e: 'v', f: 'u', g: 't',
  h: 's', i: 'r', j: 'q', k: 'p', l: 'o', m: 'n',
  n: 'm', o: 'l', p: 'k', q: 'j', r: 'i', s: 'h',
  t: 'g', u: 'f', v: 'e', w: 'd', x: 'c', y: 'b', z: 'a',
  '0': '0', '1': '1', '2': '2', '3': '3', '4': '4',
  '5': '5', '6': '6', '7': '7', '8': '8', '9': '9',
}

string.spaced = (str, len=5) ->
  -- Capture all the segments at least 1 up to 5 chars in length
  -- The Lua equivalent of the PCRE '(.{1,5})*'
  chunks = re.compile "{| { . .^-#{len-1} }* |}"
  table.concat chunks\match(str), ' '

-- --------------------------------------------------------------------
decode = (phrase) -> phrase\lower!\gsub(".", (char) -> map[char] or '')
encode = (phrase) -> (decode phrase)\spaced!

{ :encode, :decode }
