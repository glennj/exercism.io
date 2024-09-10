class BinarySearchTree
  constructor: (@data, @left = null, @right = null) ->

  insert: (value) ->
    if value <= @data
      if @left?
        @left.insert(value)
      else
        @left = new BinarySearchTree(value)
      # can be written as
      # @left?.insert(value) ? @left = new BST(value)
    else
      if @right?
        @right.insert(value)
      else
        @right = new BinarySearchTree(value)

  each: (callback) ->
    @left?.each(callback)
    callback(@data)
    @right?.each(callback)

module.exports = BinarySearchTree
