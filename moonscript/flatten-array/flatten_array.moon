  
{
  flatten: (input) ->
    flattened = {}

    flattener = (list) ->
      for elem in *list
        if type(elem) == 'table'
          flattener elem
        elseif elem != 'null'
          table.insert flattened, elem

    flattener input
    flattened
}
