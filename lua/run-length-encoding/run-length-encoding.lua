local rle = {}

local encode_run = function(char, count)
    return count == 1 and char or ("%d%s"):format(count, char)
end

--------------------------------------------------------------------
-- Other languages, those with more complete regex implmementations,
-- allow backreferences in the pattern. For example, ruby can do
--[[
      def encode(input)
        input.scan(/((.)\2*)/).reduce('') do |encoded, (run, char)|
          encoded + (run.length > 1 ? run.length.to_s : '') + char
        end
      end
--]]
-- However, Lua's patterns are simpler, so we iterate over the chars.
--------------------------------------------------------------------
rle.encode = function(input)
    local runs = {}
    local prev = ""
    local count = 0

    for i = 1, #input do
        local char = input:sub(i,i)
        if char == prev then
            count = count + 1
        else
            if prev ~= "" then
                runs[#runs+1] = encode_run(prev, count)
            end
            prev = char
            count = 1
        end
        if i == #input then     -- last run
            runs[#runs+1] = encode_run(char, count)
        end
    end
    return table.concat(runs)
end

--------------------------------------------------------------------
rle.decode = function(input)
    local decoded = ""
    for n, c in input:gmatch("(%d*)(%D)") do
        decoded = decoded .. c:rep(n == "" and 1 or tonumber(n))
    end
    return decoded
end

--------------------------------------------------------------------
return rle


--[[ https://exercism.io/tracks/lua/exercises/run-length-encoding/solutions/1bd86fef3c9a42cab0e967bc635655d9 
-- clever community solution: use `gsub` for runs of 2 or more
--
        function rle.encode(decoded)
            for character in decoded:gmatch(".") do
                decoded = decoded:gsub(character:rep(2) .. "+", function(match)
                    return tostring(#match) .. character 
                end)
            end
            return decoded
        end

        function rle.decode(encoded)
            return encoded:gsub("(%d+)(.)", function(length, character)
                return character:rep(length)
            end)
        end
--]]
