local Animal = {}
function Animal:new(name)
    local animal = {}
    setmetatable(animal, self)
    self.__index = self
    animal.name = name
    animal.tag = ""
    animal.extra = ""
    animal.apex_predator = false
    return animal
end

function Animal:set_tag(phrase)
    self.tag = phrase
    return self
end

function Animal:set_extra(phrase)
    self.extra = phrase
    return self
end

function Animal:killer()
    self.apex_predator = true
    return self
end

----------------------------------------

local chain = {}

table.insert(chain, Animal:new('fly'))
table.insert(chain, Animal:new('spider'):set_tag('It wriggled and jiggled and tickled inside her.\n'):set_extra(' that wriggled and jiggled and tickled inside her'))
table.insert(chain, Animal:new('bird'):set_tag('How absurd to swallow a bird!\n'))
table.insert(chain, Animal:new('cat'):set_tag('Imagine that, to swallow a cat!\n'))
table.insert(chain, Animal:new('dog'):set_tag('What a hog, to swallow a dog!\n'))
table.insert(chain, Animal:new('goat'):set_tag('Just opened her throat and swallowed a goat!\n'))
table.insert(chain, Animal:new('cow'):set_tag('I don\'t know how she swallowed a cow!\n'))
table.insert(chain, Animal:new('horse'):killer():set_tag('She\'s dead, of course!\n'))

----------------------------------------

local function i_know(animal)
    return ("I know an old lady who swallowed a %s.\n%s"):format(
        animal.name, animal.tag
    )
end

local function hunt(predator, prey)
    return ("She swallowed the %s to catch the %s%s.\n"):format(
        predator.name, prey.name, prey.extra
    )
end

local function i_dont_know()
    return "I don't know why she swallowed the fly. Perhaps she'll die.\n"
end

----------------------------------------

local function verse(n)
    if n < 1 or n > #chain then error('Invalid verse number') end
    local animal = chain[n]
    local verse = i_know(animal)
    if not animal.apex_predator then
        for i = n, 2, -1 do
            verse = verse .. hunt(chain[i], chain[i-1])
        end
        verse = verse .. i_dont_know()
    end
    return verse
end

local function verses(from, to)
    local verses = ""
    for i = from, to do
        verses = verses .. verse(i) .. "\n"
    end
    return verses
end

local function sing()
    return verses(1, #chain)
end

----------------------------------------

return {
    verse = verse,
    verses = verses,
    sing = sing,
}
