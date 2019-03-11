local function flatten (input, flattened)
    flattened = flattened or {}
    for i, element in ipairs(input) do
        if type(element) == "table" then
            flatten(element, flattened)
        else
            table.insert(flattened, element)
        end
    end
    return flattened
end

return flatten
