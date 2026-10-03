type drop = {
  n: int,
  sound: string,
}
let drops = [{n: 3, sound: "Pling"}, {n: 5, sound: "Plang"}, {n: 7, sound: "Plong"}]

let convert = number => {
  let sounds =
    drops
    ->Array.filter(d => number % d.n == 0)
    ->Array.map(d => d.sound)
    ->Array.join("")

  if String.isEmpty(sounds) {Int.toString(number)} else {sounds}
}
