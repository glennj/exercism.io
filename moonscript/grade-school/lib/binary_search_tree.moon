class BST
  new: =>
    @value = nil

  add: (value) =>
    if @value == nil
      @value = value
    else if value < @value
      @left = @left or BST!
      @left\add value
    else if value > @value
      @right = @right or BST!
      @right\add value

  foreach: (fn) =>
    if @value != nil
      @left\foreach fn if @left
      fn @value
      @right\foreach fn if @right

  reduce: (acc, fn) =>
    @foreach (value) -> acc = fn acc, value
    acc
