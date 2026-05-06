return {
    square = function(n)
        assert(1 <= n and n <= 64, 'square must be between 1 and 64')
        if n < 64 then
            -- these numbers can be exactly represented by a 
            -- *signed* 64-bit int
            return 1 << (n-1)
        else
            return 2^63
        end
    end,

    total = function()
        -- an imprecise float.
        -- Lua represents 18446744073709551615 as 18446744073709551616.0
        return (2^64) - 1
    end,
}
