local School = {}
School.__index = School

function School:new()
    local school = {students = {}}
    setmetatable(school, self)
    return school
end

function School:add(name, grade)
    if self.students[name] then
        return false
    end
    self.students[name] = grade
    return true
end

function School:roster()
    local names = {}
    for name, _ in pairs(self.students) do
        names[#names + 1] = name
    end
    table.sort(names, function(a, b)
        local aGrade, bGrade = self.students[a], self.students[b]
        return aGrade < bGrade or (aGrade == bGrade and a < b)
    end)
    return names
end

function School:grade(wanted)
    local names = {}
    for name, grade in pairs(self.students) do
        if grade == wanted then
            names[#names + 1] = name
        end
    end
    table.sort(names)
    return names
end

return School
