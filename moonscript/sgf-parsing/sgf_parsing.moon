lpeg = require 'lpeg'
import P, C, R, S, V, Ct, Cg from lpeg

-- `P"("` is idiomatic Lua for `P("(")`
openParen = P'('
closeParen = P')'
semicolon = P';'
openBracket = P'['
closeBracket = P']'
nl = P'\\\n' / '' -- escaped newline replaced with nothing
tab = P'\\\t' / ' ' -- escaped tab replaced with space
esc = P'\\' / '' * C 1 -- any escaped character
whitespace = S' \t\r' / ' '  -- only horizontal whitespace
nonCloseBracket = C(1 - closeBracket)  -- any character except unescaped close bracket
propValue = nl + tab + esc + whitespace + nonCloseBracket

SGFParser = P {
  'GameTree'
  GameTree: openParen * V'NodeSequence' * closeParen
  NodeSequence: Ct semicolon * V'Properties' * V'Children'

  Properties: Cg Ct(V'Property'^0), 'properties'
  Children: Cg Ct((V'NodeSequence' + V'GameTree'^1)^-1), 'children'

  -- at least one property value is required, but allow zero so we can detect missing delimiters
  Property: C(V'PropIdent') * Ct(V'PropValue'^0)
  -- property identifiers must be uppercase letters, but allow lowercase so we can detect them
  PropIdent: R('AZ', 'az')^1
  PropValue: openBracket * Ct(propValue^0) / table.concat * closeBracket
}

-- lpeg table capture (Ct) can only capture sequences.
-- remap_properties converts a sequence of name-value pairs into a map of name:value.
remap_properties = (tree) ->
  props = {}
  for i = 1, #tree.properties, 2
    name, values = tree.properties[i], tree.properties[i + 1]
    assert name == name\upper!, 'property must be in uppercase'
    assert #values > 0, 'properties without delimiter'
    props[name] = values
  tree.properties = props
  -- recursively descend into the tree's children.
  remap_properties child for child in *tree.children
  tree

{
  parse: (input) ->
    -- a couple of checks that the parse cannot perform
    assert input\match('^%(.-%)$'), 'tree missing'
    assert input\match(';'), 'tree with no nodes'

    gameTree = SGFParser\match input
    assert gameTree, 'cannot parse input'
    remap_properties gameTree
}