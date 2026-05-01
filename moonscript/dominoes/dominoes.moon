List = require 'pl.List'

isChain = (chain) ->
  return true if chain\len! == 0
  prev = chain[#chain][2]
  for d in *chain
    return false if d[1] != prev
    prev = d[2]
  true

-- this also returns the chain so the caller can do something with it
buildChain = (chain, rest) ->
  if rest\len! == 0
    return isChain(chain), chain

  last = chain[#chain][2]
  for i, d in ipairs rest
    if last == d[1]
      ok, c = buildChain chain\clone!\append(d), rest\clone!\remove(i)
      return true, c if ok
    if last == d[2]
      ok, c = buildChain chain\clone!\append({d[2], d[1]}), rest\clone!\remove(i)
      return true, c if ok
  false, nil
        

{
  canChain: (dominoes) ->
    ds = List dominoes
    if ds\len! == 0
      true
    else
      ok, _ = buildChain ds\slice(1,1), ds\slice(2)
      ok
}
