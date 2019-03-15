local map = function(list, func)
    local result = {}
    for i,elem in ipairs(list) do
        result[i] = func(elem)
    end
    return result
end

local foreach = function(list, func, ...)
    for _, elem in ipairs(list) do
        elem[func](...)
    end
end

------------------------------------------------------------
-- parent of InputCell and ComputeCell
--
local Cell = function()
    local value
    local listeners = {}
    return {
        store_value = function(val) value = val end,

        get_value = function() return value end,

        add_listener = function(listener)
            listeners[#listeners+1] = listener
        end,

        recompute_listeners = function()
            foreach(listeners, "recompute")
        end,

        fire_listener_callbacks = function()
            foreach(listeners, "fire_callbacks")
        end,
    }
end

------------------------------------------------------------
local InputCell = function(val)
    local self = Cell()
    self.store_value(val)

    self.set_value = function(val)
        self.store_value(val)
        self.recompute_listeners()
        self.fire_listener_callbacks()
    end

    return self
end

------------------------------------------------------------
local ComputeCell = function(...)
    local self = Cell()
    local previous
    local inputs = {...}
    local formula = table.remove(inputs)
    local callbacks = {}

    local compute = function()
        local values = map(inputs, function(input)
            return input.get_value()
        end)
        self.store_value(formula(table.unpack(values)))
    end

    -- initialize
    foreach(inputs, "add_listener", self)
    compute()
    previous = self.get_value()

    -- instance methods
    self.recompute = function()
        compute()
        self.recompute_listeners()
    end

    self.fire_callbacks = function()
        local val = self.get_value()
        if previous ~= val then
            previous = val
            for _,callback in ipairs(callbacks) do
                callback(val)
            end
            self.fire_listener_callbacks()
        end
    end

    self.watch = function(callback)
        callbacks[#callbacks+1] = callback
    end

    self.unwatch = function(callback)
        for i, c in ipairs(callbacks) do
            if callback == c then
                table.remove(callbacks, i)
                break
            end
        end
    end

    return self
end

------------------------------------------------------------

return {
    Reactor = function()
        return {
            InputCell = InputCell,
            ComputeCell = ComputeCell,
        }
    end,
}
