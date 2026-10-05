let alphabet = "abcdefghijklmnopqrstuvwxyz"

let splitMapJoin = (text: string, separator: string, func: string => string): string =>
  text->String.split(separator)->Array.map(func)->Array.join(separator)

let rotate = (text, rotation) => {
  let rotated =
    String.substring(alphabet, ~start=rotation) +
    String.substring(alphabet, ~start=0, ~end=rotation)
  let in_ = alphabet + String.toUpperCase(alphabet)
  let out_ = rotated + String.toUpperCase(rotated)

  splitMapJoin(text, "", char =>
    switch String.indexOf(in_, char) {
    | -1 => char
    | i => String.charAt(out_, i)
    }
  )
}
