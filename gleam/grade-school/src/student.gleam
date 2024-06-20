import gleam/int
import gleam/order
import gleam/string

pub type Student {
  StudentValue(name: String, grade: Int)
}

pub fn new(name: String, grade: Int) {
  StudentValue(name, grade)
}

pub fn compare(a: Student, b: Student) {
  case int.compare(a.grade, b.grade) {
    order.Eq -> string.compare(a.name, b.name)
    cmp -> cmp
  }
}
