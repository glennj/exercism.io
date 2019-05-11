typealias Student = String
typealias Grade   = Int
typealias Roster  = [Grade: [Student]]

class GradeSchool {
    private(set) var roster: Roster
    
    init() { self.roster = [:] }

    func addStudent(_ name: Student, grade g: Grade) -> Void {
        var grade = self.studentsInGrade(g)
        grade.append(name)
        self.roster[g] = grade
    }

    func studentsInGrade(_ g: Grade) -> [Student] {
        return self.roster[g] ?? []
    }

    var sortedRoster: Roster {
        get {
            var sorted: Roster = [:]
            for (grade, students) in self.roster {
                sorted[grade] = students.sorted()
            }
            return sorted
        }
    }
}