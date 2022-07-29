/* this implementation stores the roster as a map of
 *    (gradeNum : sorted list of names)
 */

import "./string-utils" for StringUtils

class GradeSchool {
  construct new() {
    _roster = {}
  }

  roster { grades.reduce([]) {|result, grade| result + _roster[grade]} }

  grades { _roster.keys.toList.sort() }

  // returns a copy, not a reference
  grade(grade) { (_roster[grade] || [])[0..-1] }

  addAll(students) { students.map {|s| add(s[0], s[1])}.toList }

  add(name, grade) {
    if (_roster.values.any {|cls| cls.contains(name)}) return false

    _roster[grade] = this.grade(grade) + [name]
    StringUtils.sort(_roster[grade])
    return true
  }
}
