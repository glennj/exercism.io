module TisburyTreasureHunt

let getCoordinate (line: string * string): string =
    snd line

let convertCoordinate (coordinate: string): int * char = 
    let char2int char = int char - int '0'
    (char2int coordinate[0], coordinate[1])

let compareRecords (azarasData: string * string) (ruisData: string * (int * char) * string) : bool = 
    let _, coordTuple, _ = ruisData
    coordTuple = (convertCoordinate (snd azarasData))

let createRecord (azarasData: string * string) (ruisData: string * (int * char) * string) : (string * string * string * string) =
    if compareRecords azarasData ruisData then
        let treasure, coordStr = azarasData
        let location, _, quadrant = ruisData
        (coordStr, location, quadrant, treasure)
    else
        ("", "", "", "")
