module Clock

type Clock = Clock of int

let private MinutesPerHour = 60
let private HoursPerDay = 24

let private floorMod num div = (num % div + div) % div

let private normalize hours minutes =
    floorMod (hours * MinutesPerHour + minutes) (MinutesPerHour * HoursPerDay)


let create hours minutes = Clock(normalize hours minutes)

let add minutes (Clock(myMinutes)) = create 0 (minutes + myMinutes)

let subtract minutes clock = add -minutes clock

let display (Clock(minutes)) = 
    $"%02d{minutes / MinutesPerHour}:%02d{minutes % MinutesPerHour}"
