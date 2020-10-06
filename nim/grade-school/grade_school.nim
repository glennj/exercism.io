from algorithm import sorted, sortedByIt
from sequtils  import filterIt, mapIt

type
  Student = tuple
    name: string
    grade: int

  School* = object
    students*: seq[Student]


# list of student names, sorted first by grade then by name
proc roster*(school: School): seq[string] =
  school
    .students
    .sortedByIt((it.grade, it.name))
    .mapIt(it.name)


# list of students in the requested grade, sorted by name
proc grade*(school: School, grade: int): seq[string] =
  school
    .students
    .filterIt(it.grade == grade)
    .mapIt(it.name)
    .sorted
