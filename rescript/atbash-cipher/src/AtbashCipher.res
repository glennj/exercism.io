let translation = dict{
  "a": "z", "b": "y", "c": "x", "d": "w", "e": "v", "f": "u",
  "g": "t", "h": "s", "i": "r", "j": "q", "k": "p", "l": "o", "m": "n",
  "n": "m", "o": "l", "p": "k", "q": "j", "r": "i", "s": "h", "t": "g",
  "u": "f", "v": "e", "w": "d", "x": "c", "y": "b", "z": "a",
  "0": "0", "1": "1", "2": "2", "3": "3", "4": "4",
  "5": "5", "6": "6", "7": "7", "8": "8", "9": "9",
}

let decode = ciphertext =>
  ciphertext
  ->String.toLowerCase
  ->String.split("")
  ->Array.filter(c => Dict.has(translation, c))
  ->Array.map(c => Dict.getUnsafe(translation, c))
  ->Array.join("")

let spaced = (str, ~size=5) => {
  let re = `.{1,${Int.toString(size)}}`->RegExp.fromString(~flags="g")
  switch str->String.match(re) {
  | None => str
  | Some(matches) => matches->Array.keepSome->Array.join(" ")
  }
}

let encode = plaintext => plaintext->decode->spaced
