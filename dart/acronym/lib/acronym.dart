/* A word starts with a letter and can contain word chars and apostrophes.
 * I rely on regex "leftmost longest" matching to find the first letter of each word.
 */
final wordRegex = RegExp(r"(\p{Alpha})[\w']*", unicode: true);

class Acronym {
  String abbreviate(String input) =>
      wordRegex.allMatches(input).map((m) => m.group(1)).join('').toUpperCase();
}
