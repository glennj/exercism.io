-- create a custom assertion: does a string match a pattern
assert = require 'luassert'
say = require 'say'

match = (state, arguments) ->
  {str, patt} = arguments
  string.match str, patt

say\set 'assertion.match.positive', 'Expected %s to match pattern %s.'
say\set 'assertion.match.negative', 'Expected %s not to match pattern %s.'
assert\register 'assertion', 'match', match, 'assertion.match.positive', 'assertion.match.negative'


-- return the helper functions
{
  table_size: (t) ->
    -- because `#t` is insufficient for non-sequence tables
    count = 0
    count += 1 for _ in pairs t
    count
}
