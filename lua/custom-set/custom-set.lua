local Set = {}
Set.__index = Set
setmetatable(Set, {
    -- syntactic sugar:
    -- `Set(1,2,3)` works like `Set:new(1,2,3,4)`
    __call = function(cls, ...) return cls:new(...) end,
})

-- Set constructor
-- Pass some arguments to populate the set
function Set:new(...)
    local set = {}
    setmetatable(set, self)
    set:add(...)
    return set
end

-- add some elements to the set
function Set:add(...)
    for i = 1, select('#', ...) do
        local element = select(i, ...)
        self[element] = true
    end
end

-- return the set's elements as a list
function Set:elements() 
    local elements = {}
    for elem in pairs(self) do
        elements[#elements+1] = elem
    end
    return elements
end

-- add all of another set's elements to myself
function Set:add_all(other)
    self:add(table.unpack(other:elements()))
end

-- my size
function Set:size()
    local count = 0
    for _ in pairs(self) do
        count = count + 1
    end
    return count
end

-- true if I have no elements
function Set:is_empty()
    return not next(self)
end

-- true if I contain the given element
function Set:contains(element)
    return not not self[element]
end

-- true if I am a subset of other
-- (I have no elements that other does not have)
function Set:is_subset(other)
    for elem in pairs(self) do
        if not other:contains(elem) then
            return false
        end
    end
    return true
end

-- true if other has none of my elements
function Set:is_disjoint(other)
    for elem in pairs(self) do
        if other:contains(elem) then
            return false
        end
    end
    return true
end

-- sets are subsets of each other (same size, same elements)
function Set:equals(other)
    return self:is_subset(other) and other:is_subset(self)
end

-- return a set containing the elements common to both sets
function Set:intersection(other)
    local int = Set:new()
    for elem in pairs(self) do
        if other:contains(elem) then
            int:add(elem)
        end
    end
    return int
end

-- return a set containing elements in my set and not in other
function Set:difference(other)
    local diff = Set:new()
    for elem in pairs(self) do
        if not other:contains(elem) then
            diff:add(elem)
        end
    end
    return diff
end

-- return a set containing all elements of both sets
function Set:union(other)
    local union = Set:new()
    union:add_all(self)
    union:add_all(other)
    return union
end

return Set
