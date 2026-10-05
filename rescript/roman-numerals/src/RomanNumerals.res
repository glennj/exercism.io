let roman = n => {
  let rec romanizer = (d, r) =>
    switch d {
    | _ if d >= 1000 => romanizer(d - 1000, r + "M")
    | _ if d >= 900 => romanizer(d - 900, r + "CM")
    | _ if d >= 500 => romanizer(d - 500, r + "D")
    | _ if d >= 400 => romanizer(d - 400, r + "CD")
    | _ if d >= 100 => romanizer(d - 100, r + "C")
    | _ if d >= 90 => romanizer(d - 90, r + "XC")
    | _ if d >= 50 => romanizer(d - 50, r + "L")
    | _ if d >= 40 => romanizer(d - 40, r + "XL")
    | _ if d >= 10 => romanizer(d - 10, r + "X")
    | _ if d >= 9 => romanizer(d - 9, r + "IX")
    | _ if d >= 5 => romanizer(d - 5, r + "V")
    | _ if d >= 4 => romanizer(d - 4, r + "IV")
    | _ if d >= 1 => romanizer(d - 1, r + "I")
    | _ => r
    }
  romanizer(n, "")
}
