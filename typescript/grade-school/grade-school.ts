type Roster = Map<string, string[]>

class GradeSchool {
  private roster: Roster
  constructor() { this.roster = new Map() }

  addStudent(name: string, grade: number | string): void {
    const gradeStr = grade.toString()
    const currentClass = this.roster.get(gradeStr) || []
    this.roster.set(gradeStr, currentClass.concat(name).sort())
  }

  studentsInGrade(grade: number | string): string[] {
    // take a slice to return a *copy* of the array
    return (this.roster.get(grade.toString()) || []).slice()
  }

  studentRoster(): Roster {
    const deepCopy = new Map()
    for (const [grade, students] of this.roster) {
      // take a slice to return a *copy* of the array
      deepCopy.set(grade, students.slice())
    }
    return deepCopy
  }
}

export default GradeSchool
