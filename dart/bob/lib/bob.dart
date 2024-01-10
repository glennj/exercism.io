RegExp UPPER = RegExp(r'\p{Uppercase}', unicode: true);
RegExp LOWER = RegExp(r'\p{Lowercase}', unicode: true);

class Bob {
  String response(String input) {
    var trimmed = input.trimRight();

    var isSilence = trimmed.length == 0;
    var isYelling = trimmed.contains(UPPER) && !trimmed.contains(LOWER);
    var isQuestion = trimmed.endsWith('?');

    /* switch _expressions_ require Dart v3:
     * ensure the sdk environment constraint is altered in pubspec.yaml
     */
    return switch ((isSilence, isYelling, isQuestion)) {
      (true, _, _)     => 'Fine. Be that way!',
      (_, true, true)  => "Calm down, I know what I'm doing!",
      (_, true, false) => 'Whoa, chill out!',
      (_, false, true) => 'Sure.',
      _                => 'Whatever.',
    };
  }
}
