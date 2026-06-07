append = (list1, list2) ->
  result = [elem for elem in *list1]
  result[#result + 1] = elem for elem in *list2
  result
    
foldl = (list, initial, func) ->
  accum = initial
  accum = func(accum, elem) for elem in *list
  accum

reverse = (list) ->
  foldl list, {}, (rev, elem) -> {elem, table.unpack rev} 

{
  :append
  :foldl
  :reverse
    
  concat: (lists) -> foldl lists, {}, (accum, list) -> append accum, list
  
  length: (list) -> foldl list, 0, (len, _) -> len + 1
      
  filter: (list, pred) -> [elem for elem in *list when pred elem]
  
  map: (list, func) -> [func elem for elem in *list]
  
  foldr: (list, initial, func) -> foldl reverse(list), initial, func
}
