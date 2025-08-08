let toRoman = num => {
  let digits = [
    (1000, "M"), (900, "CM"), (500, "D"), (400, "CD"),
     (100, "C"),  (90, "XC"),  (50, "L"),  (40, "XL"),
      (10, "X"),   (9, "IX"),   (5, "V"),   (4, "IV"),
       (1, "I"),
  ];
  let n = ref(num);
  let roman = ref("");

  List.iter(
    ((d, r)) => while (n^ >= d) { n := n^ - d; roman := roman^ ++ r },
    digits,
  );

  roman^;
};
