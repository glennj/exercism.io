import 'int_extensions.dart';
import 'range.dart';

const ALLERGENS = <String>[
  'eggs', 'peanuts', 'shellfish', 'strawberries',
  'tomatoes', 'chocolate', 'pollen', 'cats',
];

class Allergies {
  List<String> list(int code) =>
      Range(0, ALLERGENS.length - 1)
          .where((i) => code.isBitSetAt(i))
          .map((i) => ALLERGENS[i])
          .toList();

  bool allergicTo(String allergen, int code) =>
    list(code).contains(allergen);
}
