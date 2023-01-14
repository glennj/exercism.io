# with gratitude to
# https://exercism.org/tracks/powershell/exercises/grade-school/solutions/palpal 

class Student {
    [int]$Grade
    [string]$Name

    Student([int]$grade, [string]$name) {
        $this.Grade = $grade
        $this.Name = $name
    }
}

class Roster {
    [Student[]]$Student = @()

    [void] AddStudent([int]$grade, [string]$name) {
        $this.Student += [Student]::new($grade, $name)
    }

    [Student[]] GetRoster() {
        return $this.Student `
               | Sort-Object -Property Grade,Name
    }

    [Student[]] GetRoster([int]$grade) {
        return $this.Student `
               | Where-Object -Property Grade -eq $grade `
               | Sort-Object -Property Name
    }
}
