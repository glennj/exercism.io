-- This is where Lua's frontier pattern is really useful: look for a letter at
-- the start of a word (a letter that is not preceded by a letter).

{
  abbreviate: (phrase) ->
    no_apostrophes = phrase\gsub "(%a)'(%a)", '%1%2'
    table.concat [m\upper! for m in no_apostrophes\gmatch '%f[%a].']
}
