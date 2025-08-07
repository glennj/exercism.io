let is_isogram = phrase => {
  let is_upper = charcode => 65 <= charcode && charcode <= 90;

  let rec do_isogram = (str, seen) => {
    if (String.length(str) == 0) {
      true
    } else {
      let ch = Char.code(str.[0]);
      let rest = String.sub(str, 1, String.length(str) - 1)

      if (!is_upper(ch)) {
        do_isogram(rest, seen)
      } else if (!Belt.Set.Int.has(seen, ch)) {
        do_isogram(rest, Belt.Set.Int.add(seen, ch))
      } else {
        false
      }
    }
  }

  do_isogram(String.uppercase_ascii(phrase), Belt.Set.Int.empty)
};
