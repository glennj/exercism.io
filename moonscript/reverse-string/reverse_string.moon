import treverse from require 'lib/table'
utf8 = require 'utf8'

reverse = (str) ->
  codes = [c for _, c in utf8.codes str]
  treverse codes
  utf8.char table.unpack codes

return reverse
