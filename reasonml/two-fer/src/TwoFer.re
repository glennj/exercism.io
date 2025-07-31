let twoFer = input => {
  /*
  let name =
    switch (input) {
    | Some(name) => name
    | None => "you"
    };

  "One for " ++ name ++ ", one for me.";
  */

  "One for " ++ Belt.Option.getWithDefault(input, "you") ++ ", one for me.";
};
