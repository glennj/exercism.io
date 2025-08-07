let collatzConjecture = number => {
  let nextCollatz = n => {
    switch (n mod 2) {
    | 0 => n / 2
    | _ => n * 3 + 1
    };
  };

  let rec doCollatz = (num, steps) =>
    if (num < 1) {
      Error("Only positive integers are allowed");
    } else if (num == 1) {
      Ok(steps);
    } else {
      doCollatz(nextCollatz(num), steps + 1);
    };

  doCollatz(number, 0);
};
