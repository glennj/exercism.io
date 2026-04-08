PLANTS = V: 'violets', R: 'radishes', C: 'clover', G: 'grass'
STUDENTS = {'Alice', 'Bob', 'Charlie', 'David', 'Eve', 'Fred', 'Ginny', 'Harriet', 'Ileana', 'Joseph', 'Kincaid', 'Larry'}

index_of = (list, item) ->
  for i, value in ipairs list
    return i if value == item
   

class KindergartenGarden
  new: (diagram) =>
    rows = [ [ps for ps in row\gmatch '..'] for row in diagram\gmatch '[^\n]+']
    @plots = [rows[1][i] .. rows[2][i] for i = 1, #rows[1]]

  plants: (student) =>
    i = index_of STUDENTS, student
    [PLANTS[plant] for plant in @plots[i]\gmatch '.']
