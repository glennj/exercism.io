School = require './lib/school'

{
  roster: (list) ->
    school = School!
    school\addAll list
    school\roster!

  add: (list) ->
    School!\addAll list

  grade: (list, grade) ->
    school = School!
    school\addAll list
    school\grade grade
}
