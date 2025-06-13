module GradeSchool exposing (Grade, Result(..), School, Student, addStudent, allStudents, emptySchool, studentsInGrade)

import Dict exposing (..)


type alias Grade =
    Int


type alias Student =
    String


type alias School =
    Dict Int (List Student)


type Result
    = Added
    | Duplicate


emptySchool : School
emptySchool =
    Dict.empty


addStudent : Grade -> Student -> School -> ( Result, School )
addStudent grade student school =
    if List.member student (allStudents school) then
        ( Duplicate, school )

    else
        let
            currentClass =
                studentsInGrade grade school

            newClass =
                if List.isEmpty currentClass then
                    [ student ]

                else
                    List.sort (student :: currentClass)
        in
        ( Added, Dict.insert grade newClass school )


studentsInGrade : Grade -> School -> List Student
studentsInGrade grade school =
    case Dict.get grade school of
        Nothing -> []
        Just students -> students


allStudents : School -> List Student
allStudents school =
    school
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.concatMap Tuple.second
