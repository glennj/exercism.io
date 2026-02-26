import contains, distinct, intersection from require 'lib/table'

tree = (preorder, inorder) ->
  assert #preorder == #inorder, 'traversals must have the same length'
  assert #preorder == #distinct(preorder), 'traversals must contain unique items'
  assert #preorder == #intersection(preorder, inorder), 'traversals must have the same elements'

  if #preorder == 0 then return {}

  -- the root of the tree is the first element of the pre-ordering
  root = preorder[1]

  -- find it in the in-ordered list
  local idx
  for i, elem in ipairs inorder
    if elem == root
      idx = i
      break

  left_in  = {table.unpack inorder, 1, idx - 1}
  right_in = {table.unpack inorder, idx + 1, #inorder}

  left_pre  = {table.unpack preorder, 2, idx}
  right_pre = {table.unpack preorder, idx + 1, #preorder}

  {
    v: root,
    l: tree left_pre, left_in,
    r: tree right_pre, right_in,
  }


{ :tree }
