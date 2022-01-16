type Directory = Map<string, number>
type ClassRoom = string[]
type Roster    = {[key: number]: ClassRoom}

export class GradeSchool {
  private directory: Directory

  constructor() {
    this.directory = new Map()
  }

  add(name: string, grade: number): void {
    this.directory.set(name, grade)
  }

  grade(grade: number): ClassRoom {
    const students: ClassRoom = []
    this.directory.forEach((grade_, name) => {
      if (grade_ === grade)
        students.push(name)
    })
    return students.sort()
  }

  roster(): Roster {
    const roster: Roster = {}
    this.directory.forEach((grade, name) => {
      roster[grade] ??= []
      roster[grade].push(name)
    })
    for (const students of Object.values(roster)) {
      students.sort()
    }
    return roster
  }
}
