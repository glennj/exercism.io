def bottles:
  ["No", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten"][.]
  + " green bottle"
  + if . == 1 then "" else "s" end
  ;

def line1: "\(bottles) hanging on the wall," ;
def line3: "And if one green bottle should accidentally fall," ;
def line4: "There'll be \(. - 1 | bottles | ascii_downcase) hanging on the wall." ;

def verse: line1, line1, line3, line4 ;

[range(.startBottles; .startBottles - .takeDown; -1) | ("", verse)] | .[1:]
