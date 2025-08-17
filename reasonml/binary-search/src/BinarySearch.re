let find = (xs, x) => {
  let rec finder = (left, right) => {
    let mid = (left + right) / 2

    switch (true) {
    | _ when left > right => None
    | _ when xs[mid] < x  => finder(mid + 1, right)
    | _ when xs[mid] > x  => finder(left, mid - 1)
    | _                   => Some(mid)
    }
  };

  finder(0, Array.length(xs) - 1)
};
