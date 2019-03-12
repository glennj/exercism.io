local LinkedList = {}
LinkedList.__index = LinkedList
setmetatable(LinkedList, {
    __call = function(cls, ...) return cls:new(...) end,
})

function LinkedList:new(...)
    return setmetatable({}, self)
end

function LinkedList:push(value)
    local node = {value=value}
    if self.tail then
        node.prev = self.tail
        self.tail.next = node
    else
        self.head = node
    end
    self.tail = node
    return self
end

function LinkedList:pop()
    local value
    local node = self.tail
    if node then
        value = node.value
        if node.prev then
            node.prev.next = nil
            self.tail = node.prev
        else
            self.tail = nil
            self.head = nil
        end
    end
    return value
end

function LinkedList:unshift(value)
    local node = {value=value}
    if self.head then
        node.next = self.head
        self.head.prev = node
    else
        self.tail = node
    end
    self.head = node
    return self
end

function LinkedList:shift()
    local value
    local node = self.head
    if node then 
        value = node.value
        if node.next then
            node.next.prev = nil
            self.head = node.next
        else
            self.tail = nil
            self.head = nil
        end
    end
    return value
end

function LinkedList:count()
    local count = 0
    local node = self.head
    while node do
        count = count + 1
        node = node.next
    end
    return count
end

-- delete *all* nodes with matching value
function LinkedList:delete(value)
    local node = self.head
    while node do
        if node.value == value then
            -- unlink this node
            if node.next then
                node.next.prev = node.prev
            else
                self.tail = node.prev
            end
            if node.prev then
                node.prev.next = node.next
            else
                self.head = node.next
            end
        end
        node = node.next
    end
end

return LinkedList
