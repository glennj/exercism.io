local function same(a, b)
    if type(a) ~= type(b) then
        return false
    elseif type(a) ~= "table" then
        return a == b
    elseif rawequal(a, b) then
        return true
    else
        mta = getmetatable(a)
        mtb = getmetatable(b)
        if mta and mta == mtb and mta.__eq then
            return a == b
        end
        for k, v in pairs(a) do
            if not b[k] or not same(v, b[k]) then
                return false
            end
        end
        return true
    end        
end

local Seq = {}
Seq.__index = Seq
setmetatable(Seq, {
    __call = function(class, ...) return class:new(...) end
})

function Seq:new(seq)
    seq = seq or {}
    instance = { table.unpack(seq) }
    setmetatable(instance, self)
    return instance
end

function Seq:size()
    local count = 0
    for _ in ipairs(self) do count = count + 1 end
    return count
end
    
function Seq:contains(item)
    for _, elem in ipairs(self) do
        if same(elem, item) then return true end
    end
    return false
end

function Seq:copy()
    return Seq(self)
end

function Seq:append(elem)
    table.insert(self, elem)
    return self
end

function Seq:sorted()
    seq = {table.unpack(self)}
    table.sort(seq)
    return Seq(seq)
end

return Seq
