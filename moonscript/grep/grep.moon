List = require 'pl.List'
import fold from require 'moon'

identity = (x) -> x

-- XOR: one or the other but not both
xor = (a, b) -> (a and not b) or (b and not a)

grep_file = (filename, patt, opts) ->
  matches = List!
  lineno = 0
  for line in io.lines(filename)
    lineno += 1
    if xor opts.case_func(line)\match(patt), opts["-v"]
      return {filename} if opts["-l"]
      m = List {line}
      m\put lineno if opts["-n"]
      m\put filename if opts["-H"]
      matches\append m\join ':'
  matches

{ 
  grep: (options, pattern, ...) ->
    filenames = {...}

    opts = {opt, true for opt in *options}
    opts["-H"] = (#filenames > 1)
    opts.case_func = if opts["-i"] then string.lower else identity

    patt = opts.case_func( if opts["-x"] then "^#{pattern}$" else pattern )

    fold {List!, table.unpack filenames}, (result, filename) ->
      result\extend grep_file filename, patt, opts
}
