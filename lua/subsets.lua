-- ref https://stackoverflow.com/a/7615129/7552
local function mysplit(inputstr, sep)
  if sep == nil then
    sep = "%s"
  end
  local t = {}
  for str in string.gmatch(inputstr, "([^"..sep.."]+)") do
    table.insert(t, tonumber(str))
  end
  return t
end

local function sublists(list)
    assert(type(list) == "table", "not a list")
    if #list == 0 then
        return {}
    elseif #list == 1 then
        return {list}
    end

    local set = {}
    for i = 1,#list do
        local elem = list[i]
        local rest = {}
        for j = 1,#list do
            if i ~= j then
                table.insert(rest, list[j])
            end
        end

        local subs = sublists(rest)
        local t
        for i,s in ipairs(subs) do
            t = {table.unpack(s)}
            table.sort(t)
            set[table.concat(t, ",")] = 1

            t = {elem, table.unpack(t)}
            table.sort(t, function(a, b) return a < b end)
            set[table.concat(t, ",")] = 1
        end
    end

    local result = {}
    for k,v in pairs(set) do
        local sublist = mysplit(k, ",")
        table.insert(result, sublist)
    end
    return result
end

return {subsets = sublists}
