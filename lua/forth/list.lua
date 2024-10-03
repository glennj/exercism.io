local List = {}
List.__index = List
List.__len = function(self)
  return #self.data
end

function List:new()
  local list = {}
  setmetatable(list, self)
  list.data = {}
  return list
end

function List:toTable()
  return {table.unpack(self.data)}
end

function List:print()
  print('length:', #self)
  print('elements:', table.unpack(self.data))
end

function List:peek()
  return self.data[#self.data]
end

function List:pop()
  local value = self:peek()
  self.data = { table.unpack(self.data, 1, #self.data - 1) }
  return value
end

function List:push(value)
  self.data[#self.data + 1] = value
  return self
end

function List:shift()
  local value = self.data[1]
  self.data = { table.unpack(self.data, 2) }
  return value
end

function List:insert(idx, value)
  table.insert(self.data, idx, value)
end

function List:prepend_all(values)
  for i, value in ipairs(values) do
    self:insert(i, value)
  end
end

return List
