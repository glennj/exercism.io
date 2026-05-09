List = require 'pl.List'

each_slice = (list, length) ->
  coroutine.wrap ->
    for i = 1, #list - length + 1
      coroutine.yield list\slice i, i + length - 1

contains = (long_list, short_list) ->
  for slice in each_slice long_list, #short_list
    if slice == short_list
      return true
  false

{
  sublist: (list1, list2) ->
    l1, l2 = List(list1), List(list2)
    len1, len2 = #l1, #l2

    if     len1 == len2 and contains l1, l2 then 'equal'
    elseif len1 >  len2 and contains l1, l2 then 'superlist'
    elseif len1 <  len2 and contains l2, l1 then 'sublist'
    else 'unequal'
}
