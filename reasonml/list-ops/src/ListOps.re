/* Builtin list functionality used:
 * - List.cons
 * - list constructor []
 * - list destructuring [head, ...rest]
 */

let foldl = (lst, init, accumulator) => {
  let rec helper = (xs, acc) => {
    switch (xs) {
    | [] => acc
    | [x, ...rest] => helper(rest, accumulator(acc, x))
    };
  };

  helper(lst, init);
};

// everything else is implemented with foldl

let reverse = xs => foldl(xs, [], (acc, x) => List.cons(x, acc));

let append = (a, b) => foldl(reverse(a), b, (acc, x) => List.cons(x, acc));

let length = xs => foldl(xs, 0, (n, _) => n + 1);

let concat = listOfLists => foldl(listOfLists, [], append);

let filter = (xs, pred) => {
  foldl(xs, [], (acc, x) => pred(x) ? List.cons(x, acc) : acc) -> reverse;
};

let map = (xs, f) => {
  foldl(xs, [], (acc, x) => List.cons(f(x), acc)) -> reverse;
};

let foldr = (xs, init, accumulator) => foldl(reverse(xs), init, accumulator);
