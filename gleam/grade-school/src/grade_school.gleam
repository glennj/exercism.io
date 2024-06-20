import gleam/list
import student.{type Student}

pub type School =
  List(Student)

pub fn create() -> School {
  []
}

pub fn add(
  to school: School,
  student name: String,
  grade grade: Int,
) -> Result(School, Nil) {
  case contains(school, name) {
    True -> Error(Nil)
    False ->
      [student.new(name, grade), ..school]
      |> list.sort(student.compare)
      |> Ok()
  }
}

pub fn roster(school: School) -> List(String) {
  list.map(school, fn(student) { student.name })
}

pub fn grade(school: School, desired_grade: Int) -> List(String) {
  list.filter(school, fn(student) { student.grade == desired_grade })
  |> roster()
}

fn contains(school: School, name: String) {
  list.any(school, fn(student) { student.name == name })
}
