let is_triangle = (x, y, z) => {
  switch (List.sort(compare, [x, y, z])) {
  | [a, b, c] => a > 0 && a + b > c
  | _ => false
  }
};

let count_uniq_sides = (x, y, z) => {
  List.sort_uniq(compare, [x, y, z]) -> List.length
};

let is_classification = (x, y, z, pred) => {
  is_triangle(x, y, z) && count_uniq_sides(x, y, z) -> pred;
}

let is_equilateral = (x, y, z) => is_classification(x, y, z, n => n == 1);
let is_isosceles   = (x, y, z) => is_classification(x, y, z, n => n <= 2);
let is_scalene     = (x, y, z) => is_classification(x, y, z, n => n == 3);
