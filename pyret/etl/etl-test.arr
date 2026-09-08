use context starter2024

include file("etl.arr")

include string-dict

check "single letter":
  my-translate([string-dict: "1", [list: "A"]]) is [string-dict: "a", "1"]
end

check "single score with multiple letters":
  my-translate([string-dict: "1", [list: "A", "E", "I", "O", "U"]]) is [string-dict: "a", "1", "e", "1", "i", "1", "o", "1", "u", "1"]
end

check "multiple scores with multiple letters":
  input = [string-dict: "1", [list: "A", "E", "I", "O", "U"], "2", [list: "D", "G"]]
  expected = [string-dict: "a", "1", "e", "1", "i", "1", "o", "1", "u", "1", "d", "2", "g", "2"]
  my-translate(input) is expected
end

check "multiple scores with differing numbers of letters":
  input = [string-dict: "1", [list: "A", "E", "I", "O", "U"], 
                        "2", [list: "D", "G"], 
                        "3", [list: "B", "C", "M", "P"], 
                        "4", [list: "F", "H", "V", "W", "Y"], 
                        "5", [list: "K"], 
                        "8", [list: "J", "X"], 
                        "10", [list: "Q", "Z"]]
  expected = [string-dict: "a", "1", "e", "1", "i", "1", "o", "1", "u", "1", 
                                  "d", "2", "g", "2", 
                                  "b", "3", "c", "3", "m", "3", "p", "3",
                                  "f", "4", "h", "4", "v", "4", "w", "4", "y", "4",
                                  "k", "5", 
                                  "j", "8", "x", "8", 
                                  "q", "10", "z", "10"]
  my-translate(input) is expected
end
