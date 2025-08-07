let isPangram = phrase => {
  let is_upper = charcode => 65 <= charcode && charcode <= 90;

  let rec get_letters = (str, seen) => {
    if (String.length(str) == 0) {
      seen
    } else {
      let ch = Char.code(str.[0]);
      let rest = String.sub(str, 1, String.length(str) - 1)

      if (is_upper(ch)) {
        get_letters(rest, Belt.Set.Int.add(seen, ch))
      } else {
        get_letters(rest, seen)
      }
    }
  }

  let letters = get_letters(String.uppercase_ascii(phrase), Belt.Set.Int.empty)

  Belt.Set.Int.size(letters) == 26
};
