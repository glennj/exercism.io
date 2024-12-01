(* brute force *)
fun roman (number: int): string =
  if number < 0 orelse number >= 4000 then raise Domain

  else if number >= 1000 then  "M" ^ roman (number - 1000)
  else if number >=  900 then "CM" ^ roman (number -  900)
  else if number >=  500 then  "D" ^ roman (number -  500)
  else if number >=  400 then "CD" ^ roman (number -  400)
  else if number >=  100 then  "C" ^ roman (number -  100)
  else if number >=   90 then "XC" ^ roman (number -   90)
  else if number >=   50 then  "L" ^ roman (number -   50)
  else if number >=   40 then "XL" ^ roman (number -   40)
  else if number >=   10 then  "X" ^ roman (number -   10)
  else if number >=    9 then "IX" ^ roman (number -    9)
  else if number >=    5 then  "V" ^ roman (number -    5)
  else if number >=    4 then "IV" ^ roman (number -    4)
  else if number >=    1 then  "I" ^ roman (number -    1)
  else ""
    
