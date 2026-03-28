{
  slices: (series, length) ->
    assert length > -1, 'slice length cannot be negative'
    assert length > 0, 'slice length cannot be zero'
    
    slen = #series
    assert slen > 0, 'series cannot be empty'
    assert slen >= length, 'slice length cannot be greater than series length'

    -- a "stateful iterator": https://www.tutorialspoint.com/lua/lua_stateful_iterators.htm
    pos = 0
    iterator = ->
      pos += 1
      return nil if pos > slen - length + 1
      series\sub(pos, pos + length - 1)

    iterator
}