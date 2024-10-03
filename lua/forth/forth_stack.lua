local List = require('./list')

local ForthStack = {}
ForthStack.__index = ForthStack

function ForthStack:new()
  local fs = {}
  setmetatable(fs, self)
  self.stack = List:new()
  self.user_words = {}
  return fs
end

function ForthStack:toTable()
  return self.stack:toTable()
end

function ForthStack:push(value)
  self.stack:push(value)
end

function ForthStack:need(n)
  assert(#self.stack >= n, 'not enough elements')
end

function ForthStack:drop()
  self:need(1)
  local _ = self.stack:pop()
  return self
end

function ForthStack:dup()
  self:need(1)
  self.stack:push(self.stack:peek())
  return self
end

function ForthStack:swap()
  self:need(2)
  local b = self.stack:pop()
  local a = self.stack:pop()
  self.stack:push(b)
  self.stack:push(a)
  return self
end

function ForthStack:over()
  self:need(2)
  local b = self.stack:pop()
  local a = self.stack:peek()
  self.stack:push(b)
  self.stack:push(a)
  return self
end

function ForthStack:add()
  self:need(2)
  local b = self.stack:pop()
  local a = self.stack:pop()
  self.stack:push(a + b)
end

function ForthStack:sub()
  self:need(2)
  local b = self.stack:pop()
  local a = self.stack:pop()
  self.stack:push(a - b)
end

function ForthStack:mul()
  self:need(2)
  local b = self.stack:pop()
  local a = self.stack:pop()
  self.stack:push(a * b)
end

function ForthStack:div()
  self:need(2)
  local b = self.stack:pop()
  assert(b ~= 0, 'divide by zero')
  local a = self.stack:pop()
  self.stack:push(a // b)
end

function ForthStack:has_user_word(name)
  for k, _ in pairs(self.user_words) do
    if k == name then
      return true
    end
  end
  return false
end

function ForthStack:get_user_word(name)
  assert(self.user_words[name], 'unknown word')
  return self.user_words[name]
end

function ForthStack:define_word(words)
  local name = words:shift()
  assert(not tonumber(name), 'cannot redefine numbers')
  local body = {}
  local i
  while #words > 0 do
    local word = words:shift()
    if word == ";" then
      break
    end
    if self.user_words[word] then
      for _, w in ipairs(self.user_words[word]) do
        table.insert(body, w)
      end
    else
      table.insert(body, word)
    end
  end
  self.user_words[name] = body
end

return ForthStack
