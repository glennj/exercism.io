Student = require './lib/student'
BST = require './lib/binary_search_tree'

class School
  new: =>
    @students = BST!

  addAll: (list) =>
    -- returns a table of the result of adding each student in the list
    [@add Student tuple for tuple in *list]

  add: (student) =>
    return false if @seen student.name
    @students\add student
    true

  seen: (name) =>
    @students\reduce false, (seen, student) ->
      seen or student.name == name

  grade: (wanted) =>
    @students\reduce {}, (names, student) ->
      if student.grade == wanted
        table.insert names, student.name
      names

  roster: =>
    @students\reduce {}, (names, student) ->
      table.insert names, student.name
      names
