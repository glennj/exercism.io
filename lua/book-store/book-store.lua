local BookPrice = 800
local Discounted = {[0]=1.0, 1.0, 0.95, 0.90, 0.80, 0.75}

local contains
local optimize

local printList = function(list, level)
    level = level or 0

    for i, v in ipairs(list) do
        local value = v
        if type(v) == "table" then
            value = tostring(v)
        end

        print(string.rep("\t", level) .. i .. "\t" .. v)
        if type(v) == "table" then
            printList(v, level + 1)
        end
    end
end

local function total(basket)
    print("START")
    printList(basket)
    local bundles = {{}}
    local added

    -- add each book to a "bundle", a collection of books 
    -- with no duplicates
    for _, book in ipairs(basket) do
        added = false
        for _, bundle in ipairs(bundles) do
            if not contains(bundle, book) then
                table.insert(bundle, book)
                added = true
            end
        end
        if not added then
            table.insert(bundles, {book})
        end
    end
    print("bundles")
    printList(bundles)

    bundles = optimize(bundles)
    print("optimized")
    printList(bundles)

    local price = 0
    for _, bundle in ipairs(bundles) do
        local size = #bundle
        price = price + size * BookPrice * Discounted[size]
    end

    return price
end

contains = function(tbl, item)
    for _, elem in ipairs(tbl) do
        if elem == item then
            return true
        end
    end
    return false
end

-- optimize the bundles: two bundles of size 4 is cheaper
-- than a bundle of size 5 plus a bundle of size 3
optimize = function(bundles)
    local bundle5 = {}
    for _, bundle in ipairs(bundles) do
        if #bundle == 5 then
            table.insert(bundle5, bundle)
        end
    end
    if #bundle5 == 0 then
        return bundles
    end

    local bundle3 = {}
    for _, bundle in ipairs(bundles) do
        if #bundle == 3 then
            table.insert(bundle3, bundle)
        end
    end
    if #bundle3 == 0 then
        return bundles
    end

    local b5 = bundle5[1]
    local b3 = bundle3[1]

    for i, book in ipairs(b5) do
        if not contains(b3, book) then
            table.insert(b3, book)
            table.remove(b5, i)
            break
        end
    end

    return optimize(bundles)
end



return { total = total }
