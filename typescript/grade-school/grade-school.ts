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
    const classroom: ClassRoom = []
    this.directory.forEach((grade_, name) => {
      if (grade_ === grade)
        classroom.push(name)
    })
    return classroom.sort()
  }

  roster(): Roster {
    const roster: Roster = {}
    this.directory.forEach((grade, name) => {
      roster[grade] ??= []
      roster[grade].push(name)
    })
    for (const classroom of Object.values(roster)) {
      classroom.sort()
    }
    return roster
  }
}
