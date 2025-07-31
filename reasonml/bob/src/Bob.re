let hey = input => {
  let cleaned = String.trim(input);

  if (String.length(cleaned) == 0) {
    "Fine. Be that way!";
  } else {
    let asking = Char.equal('?', cleaned.[String.length(cleaned) - 1]);

    let upper = Js.Re.fromString("[A-Z]");
    let lower = Js.Re.fromString("[a-z]");
    let yelling =
      Js.Re.test_(upper, cleaned) && !Js.Re.test_(lower, cleaned);

    switch (asking, yelling) {
    | (true, true) => "Calm down, I know what I'm doing!"
    | (true, _) => "Sure."
    | (_, true) => "Whoa, chill out!"
    | _ => "Whatever."
    };
  };
};
