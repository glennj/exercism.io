refute = (cond, msg) -> assert not cond, msg

{
  clean: (phrase) ->
    -- remove valid non-digits
    cleaned = phrase\gsub '[-+(). ]', ''
    len = #cleaned

    assert len >= 10, 'must not be fewer than 10 digits'
    assert len <= 11, 'must not be greater than 11 digits'
    if len == 11
      cleaned = cleaned\match '^1(.+)'
      assert cleaned, '11 digits must start with 1'
    
    refute cleaned\match('%a'), 'letters not permitted'
    refute cleaned\match('%D'), 'punctuations not permitted' 
    refute cleaned\match('^0'), 'area code cannot start with zero'
    refute cleaned\match('^1'), 'area code cannot start with one'
    refute cleaned\match('^...0'), 'exchange code cannot start with zero'
    refute cleaned\match('^...1'), 'exchange code cannot start with one'

    cleaned
}
