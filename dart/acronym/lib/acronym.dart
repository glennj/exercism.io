/* A word starts with a letter and can contain word chars and apostrophes.
 * I rely on regex "leftmost longest" matching to find the first letter of each word.
 */
final WORD_RE = RegExp(r"([A-Za-z])[\w']*");

class Acronym {
  String abbreviate(String input) => WORD_RE
      .allMatches(input)
      .map((m) => m.group(1))
      .join('')
      .toUpperCase();
}
