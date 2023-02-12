module KindergartenGarden

(* A weed is just a plant growing where you don't want it to be. *)
type Plant = Clover | Grass | Radishes | Violets | Weed

let plants (diagram: string) (student: string): Plant list = 
    let idx = int (student[0] - 'A')
    let letters = 
        [ for row in (diagram.Split '\n') do
            yield! [row[2 * idx]; row[2 * idx + 1]]]
    
    letters |> List.map (function
                        | 'C' -> Clover
                        | 'G' -> Grass
                        | 'R' -> Radishes
                        | 'V' -> Violets
                        | _ -> Weed)
