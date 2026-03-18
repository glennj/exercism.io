keep    = (list, predicate) -> [elem for elem in *list when predicate elem]
discard = (list, predicate) -> keep list, (elem) -> not predicate elem

{ :keep, :discard }
