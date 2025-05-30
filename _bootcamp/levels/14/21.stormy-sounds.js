const SOUNDS = [[3, "Pling"], [5, "Plang"], [7, "Plong"]];

const divisibleBy = (num, den) => num % den === 0;

export function stormySounds(number) {
  const rain = SOUNDS.map(([n, s]) => divisibleBy(number, n) ? s : "").join("");
  return rain === "" ? number.toString() : rain;
}
