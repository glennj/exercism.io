local house = {}

local items = {
    {the = "house", that = "Jack built."},
    {the = "malt",  that = "lay in"},
    {the = "rat",   that = "ate"},
    {the = "cat",   that = "killed"},
    {the = "dog",   that = "worried"},
    {the = "cow with the crumpled horn",       that = "tossed"},
    {the = "maiden all forlorn",               that = "milked"},
    {the = "man all tattered and torn",        that = "kissed"},
    {the = "priest all shaven and shorn",      that = "married"},
    {the = "rooster that crowed in the morn",  that = "woke"},
    {the = "farmer sowing his corn",           that = "kept"},
    {the = "horse and the hound and the horn", that = "belonged to"}
}

function house.verse(n)
    local verse = "This is"
    for i = n, 1, -1 do
        local sep = i == 1 and " " or "\n"
        verse = verse .. " the " .. items[i].the .. sep .. "that " .. items[i].that
    end
    return verse
end

function house.recite()
    local verses = {}
    for i = 1, #items do
        table.insert(verses, house.verse(i))
    end
    return table.concat(verses, "\n")
end

return house
