class BinarySearchTree
  new: (items = {}) =>
    @value = nil
    @add item for item in *items

  add: (item) =>
    if @value == nil
      @value = item
    elseif item <= @value
      @left = @left or @@!
      @left\add item
    else
      @right = @right or @@!
      @right\add item

  data: =>
    {
      left: @left and @left\data! or nil
      data: @value
      right: @right and @right\data! or nil
    }

  sorted: (values = {})=>
    values = @left\sorted values if @left
    table.insert values, @value
    values = @right\sorted values if @right
    values

