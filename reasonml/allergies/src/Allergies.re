let allergens = [
  "eggs", "peanuts", "shellfish", "strawberries",
  "tomatoes", "chocolate", "pollen", "cats",
];

let toList = score => {
  let (allergies, _) = List.fold_left(
    ((allergies, i), a) => {
      ((score lsr i land 1 == 1 ? List.cons(a, allergies) : allergies), i + 1)
    },
    ([], 0),
    allergens
  );

  List.rev(allergies)
};

let isAllergicTo = (allergen, score) => {
  switch (List.find(a => a == allergen, toList(score))) {
  | exception Not_found => false
  | _ => true
  }
};
