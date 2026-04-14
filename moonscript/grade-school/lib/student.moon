class Student
  new: (pair) =>
    {@name, @grade} = pair

  __lt: (other) =>
    @grade < other.grade or (@grade == other.grade and @name < other.name)