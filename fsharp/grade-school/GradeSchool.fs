module GradeSchool

type School = Map<int, string list>

let empty: School = School []

let roster (school: School): string list =
    // Fortunately, F# maps are already sorted by key.
    school |> Map.values |> List.concat

let grade (number: int) (school: School): string list =
    school |> Map.tryFind number |> Option.defaultValue []

let private contains student school =
    school |> Map.exists (fun _grade students -> List.contains student students) 

let add (student: string) (number: int) (school: School): School =
    if school |> contains student then
        school
    else
        let students = student :: grade number school
        school |> Map.add number (List.sort students) 
