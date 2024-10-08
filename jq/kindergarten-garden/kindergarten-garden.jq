  {V: "violets", R: "radishes", C: "clover", G: "grass"}
    as $plants
| ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]
    as $students
| . as {$diagram, $student}             # example:
                                        # {diagram: "CGGCCG\NVRRVGG",
                                        #  student: "Bob"}

| $diagram / "\n"                       # ["CGGCCG", "VRRVGG"]
| map([scan("..")])                     # [["CG", "GC", "CG"], ["VR", "RV", "GG"]]
| transpose                             # [["CG", "VR"], ["GC", "RV"], ["CG", "GG"]]
| map(add / "" | map($plants[.]))       # [ ["clover", "grass", "violets", "radishes"],
                                        #   ["grass", "clover", "radishes", "violets"],
                                        #   ["clover", "grass", "grass", "grass"] ]

| .[ $students | index($student) ]      # ["grass", "clover", "radishes", "violets"],
