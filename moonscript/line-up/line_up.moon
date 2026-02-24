nth = (n) ->
  switch n % 100
    when 1, 21, 31, 41, 51, 61, 71, 81, 91 then "#{n}st"
    when 2, 22, 32, 42, 52, 62, 72, 82, 92 then "#{n}nd"
    when 3, 23, 33, 43, 53, 63, 73, 83, 93 then "#{n}rd"
    else "#{n}th"

{
  format: (name, number) ->
    "#{name}, you are the #{nth number} customer we serve today. Thank you!"
}
