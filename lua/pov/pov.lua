------------------------------------------------------------
-- list op functions

local lreverse = function(list)
  local result = {}
  for i = #list, 1, -1 do
    result[#result+1] = list[i]
  end
  return result
end

local lappend = function(receiver, giver)
  for i = 1, #giver do
    receiver[#receiver+1] = giver[i]
  end
end

local map = function(list, f)
  local result = {}
  for i,e in ipairs(list) do
    result[i] = f(e)
  end
  return result
end

------------------------------------------------------------
-- tree util functions

local name
local is_leaf
local children
local deep_copy
local add_child
local remove_child
local find

name     = function(tree) return tree[1]    end
is_leaf  = function(tree) return #tree == 1 end

children = function(tree)
  return coroutine.wrap(function()
    for i,c in ipairs(tree[2]) do
      coroutine.yield(i, c)
    end
  end)
end

deep_copy = function(tree)
  local new = { name(tree) }
  if not is_leaf(tree) then
    new[2] = {}
    for _,child in children(tree) do
      table.insert(new[2], deep_copy(child))
    end
  end
  return new
end

add_child = function (parent, child)
  if is_leaf(parent) then
    parent[2] = { child }
  else
    table.insert(parent[2], child)
  end
end

remove_child = function (parent, child_name)
  for i,child in children(parent) do
    if name(child) == child_name then
      table.remove(parent[2], i)
      -- if no more children, remove the empty next
      if #parent[2] == 0 then
        parent[2] = nil
      end
      break
    end
  end
end

-- A depth-first traversal through the tree
--
-- This returns an array of **trees** starting from
-- the root and descending to the desired node.
-- If node not found, return nil
--
find = function (tree, node_name)
  local path = { tree }
  if name(tree) == node_name then return path end
  if is_leaf(tree)       then return      end

  for _,child in children(tree) do
    local child_path = find(child, node_name)
    if child_path then
      lappend(path, child_path)
      return path
    end
  end
end

------------------------------------------------------------
-- Public API

local POV = {}

POV.pov_from = function (node_name)
  return {
    of = function (orig_tree)
      local tree = deep_copy(orig_tree)
      local path = find(tree, node_name)
      assert(path, 'No such node: ' .. node_name)

      -- Now, reparent: follow the path backwards, removing
      -- the node from its parent's children, and adding the
      -- node's parent as its own child.
      --
      local new = path[#path]
      local parent = new
      for i = #path-1, 1, -1 do
        local child = path[i]
        remove_child(child, name(parent))
        add_child(parent, child)
        parent = child
      end
      return new
    end
  }
end

POV.path_from = function (from_name)
  return {
    to = function (to_name)
      return {
        of = function (tree)
          local result = {}

          -- path from tree root to requested "from" node
          local from_path = find(tree, from_name)
          assert(from_path, 'No such node: ' .. from_name)
          lappend(result, lreverse(map(from_path, name)))

          -- path from tree root to requested "to" node
          if name(tree) ~= to_name then
            local to_path = find(tree, to_name)
            assert(to_path, 'No such node: ' .. to_name)
            -- pop the tree root so it's not shown twice
            result[#result] = nil
            lappend(result, map(to_path, name))
          end
          return result
        end
      }
    end
  }
end

return POV
