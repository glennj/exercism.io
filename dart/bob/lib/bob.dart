RegExp upper = RegExp(r'\p{Uppercase}', unicode: true);
RegExp lower = RegExp(r'\p{Lowercase}', unicode: true);

class Bob {
  String response(String input) {
    var trimmed = input.trimRight();

    var isSilence = trimmed.isEmpty;
    var isYelling = trimmed.contains(upper) && !trimmed.contains(lower);
    var isQuestion = trimmed.endsWith('?');

    /* switch _expressions_ require Dart v3:
     * ensure the sdk environment constraint is altered in pubspec.yaml
     */
    return switch ((isSilence, isYelling, isQuestion)) {
      (true, _, _) => 'Fine. Be that way!',
      (_, true, true) => "Calm down, I know what I'm doing!",
      (_, true, false) => 'Whoa, chill out!',
      (_, false, true) => 'Sure.',
      _ => 'Whatever.',
    };
  }
}
