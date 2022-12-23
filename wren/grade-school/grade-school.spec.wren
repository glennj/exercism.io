import "wren-testie/testie" for Testie, Expect
import "./grade-school" for GradeSchool

Testie.test("Grade School") { |do, skip|
  do.test("Roster is empty when no student is added") {
    var school = GradeSchool.new()
    Expect.value(school.roster).toEqual([])
  }

  skip.test("Add a student") {
    var school = GradeSchool.new()
    Expect.value(school.add("Aimee", 2)).toBe(true)
    Expect.value(school.roster).toEqual(["Aimee"])
  }

  skip.test("Adding multiple students in the same grade in the roster") {
    var school = GradeSchool.new()
    Expect.value(school.add("Blair", 2)).toBe(true)
    Expect.value(school.add("James", 2)).toBe(true)
    Expect.value(school.add("Paul", 2)).toBe(true)
    Expect.value(school.roster).toEqual(["Blair", "James", "Paul"])
  }

  skip.test("Cannot add student to same grade in the roster more than once") {
    var school = GradeSchool.new()
    Expect.value(school.addAll([
      ["Blair", 2],
      ["James", 2],
      ["James", 2],
      ["Paul", 2],
    ])).toBe([true, true, false, true])
    Expect.value(school.roster).toEqual(["Blair", "James", "Paul"])
  }

  skip.test("Adding students in multiple grades") {
    var school = GradeSchool.new()
    Expect.value(school.add("Chelsea", 3)).toBe(true)
    Expect.value(school.add("Logan", 7)).toBe(true)
    Expect.value(school.roster).toEqual(["Chelsea", "Logan"])
  }

  skip.test("Cannot add same student to multiple grades in the roster") {
    var school = GradeSchool.new()
    Expect.value(school.addAll([
      ["Blair", 2],
      ["James", 2],
      ["James", 3],
      ["Paul", 3],
    ])).toBe([true, true, false, true])
    Expect.value(school.roster).toEqual(["Blair", "James", "Paul"])
  }

  skip.test("Students are sorted by grades in the roster") {
    var school = GradeSchool.new()
    school.addAll([["Jim", 3], ["Peter", 2], ["Anna", 1]])
    Expect.value(school.roster).toEqual(["Anna", "Peter", "Jim"])
  }

  skip.test("Students are sorted by name in the roster") {
    var school = GradeSchool.new()
    school.addAll([["Peter", 2], ["Zoe", 2], ["Alex", 2]])
    Expect.value(school.roster).toEqual(["Alex", "Peter", "Zoe"])
  }

  skip.test("Students are sorted by grades and then by name in the roster") {
    var school = GradeSchool.new()
    school.addAll([["Peter", 2], ["Anna", 1], ["Barb", 1], ["Zoe", 2], ["Alex", 2], ["Jim", 3], ["Charlie", 1]])
    Expect.value(school.roster).toEqual(["Anna", "Barb", "Charlie", "Alex", "Peter", "Zoe", "Jim"])
  }

  skip.test("Grade is empty if no students in the roster") {
    var school = GradeSchool.new()
    Expect.value(school.grade(1)).toEqual([])
  }

  skip.test("Grade is empty if no students in that grade") {
    var school = GradeSchool.new()
    school.addAll([["Peter", 2], ["Zoe", 2], ["Alex", 2], ["Jim", 3]])
    Expect.value(school.grade(1)).toEqual([])
  }

  skip.test("Student not added to same grade more than once") {
    var school = GradeSchool.new()
    school.addAll([["Blair", 2], ["James", 2], ["James", 2], ["Paul", 2]])
    Expect.value(school.grade(2)).toEqual(["Blair", "James", "Paul"])
  }

  skip.test("Student not added to multiple grades") {
    var school = GradeSchool.new()
    school.addAll([["Blair", 2], ["James", 2], ["James", 3], ["Paul", 3]])
    Expect.value(school.grade(2)).toEqual(["Blair", "James"])
  }

  skip.test("Student not added to other grade for multiple grades") {
    var school = GradeSchool.new()
    school.addAll([["Blair", 2], ["James", 2], ["James", 3], ["Paul", 3]])
    Expect.value(school.grade(3)).toEqual(["Paul"])
  }

  skip.test("Students are sorted by name in a grade") {
    var school = GradeSchool.new()
    school.addAll([["Franklin", 5], ["Bradley", 5], ["Jeff", 1]])
    Expect.value(school.grade(5)).toEqual(["Bradley", "Franklin"])
  }
}
