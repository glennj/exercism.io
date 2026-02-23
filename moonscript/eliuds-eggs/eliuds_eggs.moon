count_eggs = (num, count = 0) ->
  while num > 0
    count += num & 1
    num = num >> 1
  count

count_eggs
