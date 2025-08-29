local BookPrice = 800
local Discounted = {[0] = 1.0, 1.0, 0.95, 0.90, 0.80, 0.75}

local bundleBooks, optimize, contains

local total = function(basket)
    local price = 0
    for _, bundle in ipairs(optimize(bundleBooks(basket))) do
        price = price + #bundle * BookPrice * Discounted[#bundle]
    end

    return price
end

-- ------------------------------------------------------------
-- add each book to a "bundle", a collection of books with no duplicates
bundleBooks = function(basket)
    local bundles = {{}}
    local added

    for _, book in ipairs(basket) do
        added = false
        for i, bundle in ipairs(bundles) do
            if not contains(bundle, book) then
                table.insert(bundle, book)
                added = true
                break
            end
        end
        if not added then
            table.insert(bundles, {book})
        end
    end
    return bundles
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
    local b5, b3
    for _, bundle in ipairs(bundles) do
        if     #bundle == 5 then b5 = bundle
        elseif #bundle == 3 then b3 = bundle
        end
    end

    if not (b5 and b3) then
        return bundles
    end

    for i, book in ipairs(b5) do
        if not contains(b3, book) then
            table.insert(b3, book)
            table.remove(b5, i)
            break
        end
    end

    -- keep repeating until no more (5-bundle plus 3-bundle) remain
    return optimize(bundles)
end

-- ------------------------------------------------------------
return { total = total }
