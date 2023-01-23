local CircularBuffer = {
    --[[
        This can easily be implemented with an array, where
        the read pointer is fixed to the head of the array and
        the write pointer is fixed to the tail of the array.
        But let's implement a proper circular tape with a read
        pointer and write pointer that move along the tape.

        I explicitly check a value is or isn't nil to allow
        `false` as valid data.
    ---]]

    new = function(self, size)
        local buf = { size = size }
        setmetatable(buf, self)
        buf:clear()
        return buf
    end,

    clear = function(self)
        self.data = {}
        self.pointer = {read = 1, write = 1}
        return self
    end,

    is_empty = function(self)
        return self.data[self.pointer.read] == nil
    end,

    is_full = function(self)
        return self.pointer.read == self.pointer.write
          and not self:is_empty()
    end,

    read = function(self)
        assert(not self:is_empty(), 'buffer is empty')
        local val = self.data[self.pointer.read]
        self.data[self.pointer.read] = nil
        self:incr_pointer('read')
        return val
    end,

    write = function(self, data)
        -- It is unspecified what to do when we write nil to a 
        -- full buffer: ignore or error. I choose ignore.
        if data ~= nil then 
            assert(not self:is_full(), 'buffer is full')
            self.data[self.pointer.write] = data
            self:incr_pointer('write')
        end
        return self
    end,

    forceWrite = function(self, data)
        -- again unspecified: I do nothing for force write nil
        -- to a full buffer
        if data ~= nil then
            if self:is_full() then 
                self:read() -- clear a space
            end
            self:write(data)
        end
        return self
    end,

    incr_pointer = function(self, ptype)
        assert(ptype == 'read' or ptype == 'write')
        self.pointer[ptype] = (self.pointer[ptype] % self.size) + 1
    end
}

CircularBuffer.__index = CircularBuffer
return CircularBuffer
