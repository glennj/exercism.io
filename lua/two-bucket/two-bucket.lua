local function gcd(a, b)
  return b == 0 and a or gcd(b, a % b)
end

-- ------------------------------------------------------------
local Bucket = {}
Bucket.__index = Bucket

function Bucket:new(id, size)
    return setmetatable({id = id, size = size, amount = 0}, self)
end
function Bucket:is_full()
    return self.amount == self.size
end
function Bucket:is_empty()
    return self.amount == 0
end
function Bucket:fill()
    self.amount = self.size
end
function Bucket:empty()
    self.amount = 0
end
function Bucket:pour_into(other)
    local quantity = math.min(self.amount, other.size - other.amount)
    self.amount = self.amount - quantity
    other.amount = other.amount + quantity
end
function Bucket:__tostring()
    local result = "Bucket("
    local sep = ""
    for k,v in pairs(self) do
        result = result .. sep .. k .. "=" .. v
        sep = ", "
    end
    return result .. ")"
end

-- ------------------------------------------------------------
local TwoBucket = {}
TwoBucket.__index = TwoBucket

function TwoBucket:new(start, other, goal)
    assert(goal <= math.max(start.size, other.size))
    local _gcd = gcd(start.size, other.size)
    assert(_gcd == 1 or goal % _gcd == 0)

    local tb = {moves = 0, start = start, other = other, goal = goal}
    return setmetatable(tb, self)
end

function TwoBucket:incr()
    self.moves = self.moves + 1
end

function TwoBucket:result(winner, loser)
    return {
        moves = self.moves,
        goal_bucket_number = winner.id,
        other_bucket_volume = loser.amount
    }
end

function TwoBucket:solve()
    self.moves = 0

    self.start:fill()
    self:incr()

    if self.other.size == self.goal then
        self.other:fill()
        self:incr()
    end

    while true do
        -- print(tostring(self))
        if self.start.amount == self.goal then
            return self:result(self.start, self.other)
        end
        if self.other.amount == self.goal then
            return self:result(self.other, self.start)
        end

        if     self.start:is_empty() then self.start:fill()
        elseif self.other:is_full()  then self.other:empty()
        else                         self.start:pour_into(self.other)
        end
        self:incr()
    end
end

function TwoBucket:__tostring()
    return "moves: " .. self.moves .. ", goal: " .. self.goal .. ", start: " .. tostring(self.start) .. ", other: " .. tostring(self.other)
end

-- ------------------------------------------------------------
return {
    measure = function (args)
        local start = Bucket:new(1, args.bucket_one_capacity)
        local other = Bucket:new(2, args.bucket_two_capacity)
        if args.start_bucket == 2 then
            start, other = other, start
        end
        -- print("===")
        local game = TwoBucket:new(start, other, args.goal_volume)
        return game:solve()
    end
}
