List<List<E>> permutations<E>(List<E> list) {
  switch (list.length) {
    case 0:
      return [];
    case 1:
      return [list];
    default:
      return list.fold([], (perms, elem) {
        var rest = list.where((e) => e != elem).toList();
        permutations(rest).forEach((p) => perms.add([elem] + p));
        return perms;
      });
  }
}

extension on int {
  bool rightOf(int other) => this == other + 1;
  bool nextTo(int other) => this == other + 1 || this == other - 1;
}

class ZebraPuzzle {
  static const HOUSES = [1, 2, 3, 4, 5];          /* clue  1 */
  static const FIRST = 1;
  static const MIDDLE = 3;

  String drinksWater = "";
  String ownsZebra = "";

  void solve() {
    // colours ----------------------------------------------
    for (final p1 in permutations(HOUSES)) {
      var [red, green, ivory, yellow, blue] = p1;
      if (green.rightOf(ivory)                    /* clue  6 */
      ) {

        // nationalities ------------------------------------
        for (final p2 in permutations(HOUSES)) {
          var [en, sp, uk, nw, jp] = p2;

          if (en == red                           /* clue  2 */
              && nw == FIRST                      /* clue 10 */
              && nw.nextTo(blue)                  /* clue 15 */
          ) {
            var nationalities = <int, String>{
              en: "Englishman",
              sp: "Spaniard",
              uk: "Ukranian",
              nw: "Norwegian",
              jp: "Japanese",
            };

            // beverages ------------------------------------
            for (final p3 in permutations(HOUSES)) {
              var [coffee, tea, milk, juice, water] = p3;

              if (coffee == green                 /* clue  4 */
                  && uk == tea                    /* clue  5 */
                  && milk == MIDDLE               /* clue  9 */
              ) {

                // cigarettes -------------------------------
                for (final p4 in permutations(HOUSES)) {
                  var [gold, kool, ches, luck, parl] = p4;

                  if (kool == yellow              /* clue  8 */
                      && luck == juice            /* clue 13 */
                      && jp == parl               /* clue 14 */
                  ) {

                    // pets ---------------------------------
                    for (final p5 in permutations(HOUSES)) {
                      var [dog, snail, fox, horse, zebra] = p5;

                      if (sp == dog               /* clue  3 */
                          && gold == snail        /* clue  7 */
                          && ches.nextTo(fox)     /* clue 11 */
                          && kool.nextTo(horse)   /* clue 12 */
                      ) {
                        drinksWater = nationalities[water]!;
                        ownsZebra = nationalities[zebra]!;
                        return;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
