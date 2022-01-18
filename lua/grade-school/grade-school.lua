local School = {}
School.__index = School

function School:new()
    local school = { grades = {} }
    setmetatable(school, self)
    return school
end

function School:roster()
    return self.grades
end

function School:add(student, n)
    local grade = self.grades[n] or {}
    table.insert(grade, student)
    table.sort(grade)
    self.grades[n] = grade
end

function School:grade(n)
    return self.grades[n] or {}
end

return School
