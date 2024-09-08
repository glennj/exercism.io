class GradeSchool
  # sorting function, first by grade, then by name
  compareStudents = (a, b) -> a.grade - b.grade or a.name.localeCompare b.name
  
  constructor: () ->
    @directory = []

  add: (student, level) -> 
    for entry in @directory
      return false if entry.name is student
    @directory.push {'name': student, 'grade': level}
    @directory.sort compareStudents    # maintain the directory in sorted order
    true

  roster: () ->
    entry.name for entry in @directory

  grade: (level) ->
    entry.name for entry in @directory when entry.grade is level

module.exports = GradeSchool
