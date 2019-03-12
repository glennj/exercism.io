local BinarySearchTree = {}
BinarySearchTree.__index = BinarySearchTree

function BinarySearchTree:new(value)
    return setmetatable({value = value}, self)
end

function BinarySearchTree:insert(value)
    if value <= self.value then
        if self.left then
            self.left:insert(value)
        else
            self.left = BinarySearchTree:new(value)
        end
    else
        if self.right then
            self.right:insert(value)
        else
            self.right = BinarySearchTree:new(value)
        end
    end
end

function BinarySearchTree:from_list(list)
    assert(type(list) == 'table' and #list > 0)
    local tree
    for i,v in ipairs(list) do
        if i == 1 then
            tree = BinarySearchTree:new(v)
        else
            tree:insert(v)
        end
    end
    return tree
end

function BinarySearchTree:each(func)
    if self.left then
        self.left:each(func)
    end
    func(self)
    if self.right then
        self.right:each(func)
    end
end

function BinarySearchTree:values()
    return coroutine.wrap(function()
        -- brazenly stolen from fyrchik
        -- https://exercism.io/tracks/lua/exercises/binary-search-tree/solutions/addfa729725d4f40bc408f2cf092e087
        self:each(function(tree)
            coroutine.yield(tree.value) 
        end)
    end)
end

return BinarySearchTree
