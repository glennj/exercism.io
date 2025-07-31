let raindrops = num => {
  let drops =
    Belt.List.reduce(
      [(3, "Pling"), (5, "Plang"), (7, "Plong")],
      "",
      (ds, (div, d)) => ds ++ (num mod div == 0 ? d : "")
    );

  String.length(drops) > 0 ? drops : Belt.Int.toString(num);
};
