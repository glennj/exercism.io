pair_removal = (input) ->
  brackets = input\gsub '[^{}%(%)%[%]]', '' -- remove all non-brackets
  while true
    prev = brackets
    brackets = brackets\gsub('{}', '')\gsub('%(%)', '')\gsub('%[%]', '')
    break if brackets == prev
  #brackets == 0


push = table.insert
pop  = table.remove

iterative = (input) ->
  stack = {}
  for char in input\gmatch '.'
    switch char
      when '{', '[', '(' then push stack, char
      when '}' then return false if '{' != pop stack
      when ']' then return false if '[' != pop stack
      when ')' then return false if '(' != pop stack
  #stack == 0


{
  -- is_paired: pair_removal
  is_paired: iterative
}
