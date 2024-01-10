String translate(String phrase) =>
  phrase.replaceAllMapped(RegExp(r'\S+'), translateMatch);

/* These regexes all capture a leading prefix and the rest of the string.
 * The pig-latin-ized translation is "{rest}{prefix}ay"
 */
final Q = RegExp(r'^([^aeiou]?qu)(.*)');
final Y = RegExp(r'^([^aeiou]+)(y.*)');
final CONSONANTS = RegExp(r'^([^aeiou]+)(.*)');
// for this one, the prefix is empty
final VOWEL = RegExp(r'^()((?:[aeiou]|xr|yt).*)');

final RE = [VOWEL, Q, Y, CONSONANTS];   // use this order

String translateMatch(Match match) {
  var word = match[0]!;
  for (var re in RE) {
    var m = re.firstMatch(word);
    if (m != null)
      return '${m[2]!}${m[1]!}ay';
  }

  // are there other cases?
  return '${word}ay';
}
