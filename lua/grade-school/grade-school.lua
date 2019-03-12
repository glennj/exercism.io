local School = {}
School.__index = School

function School:new()
    local school = {grades = {}}
    setmetatable(school, self)
    return school
end

function School:roster()
    return self.grades
end

function School:add(student, grade_no)
    local grade = self.grades[grade_no] or {}
    table.insert(grade, student)
    table.sort(grade)
    self.grades[grade_no] = grade
end

function School:grade(grade_no)
    return self.grades[grade_no] or {}
end

return School

-- Improvements:
-- * return a _copy_ of the data, not a reference
--   https://exercism.io/tracks/lua/exercises/grade-school/solutions/e88d0e057cac4678a16a1299fd545bfb 
