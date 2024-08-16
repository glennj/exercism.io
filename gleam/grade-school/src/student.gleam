import gleam/int
import gleam/order
import gleam/string

pub type Student {
  Student(name: String, grade: Int)
}

pub fn compare(a: Student, b: Student) {
  order.break_tie(
    in: int.compare(a.grade, b.grade),
    with: string.compare(a.name, b.name),
  )
}
