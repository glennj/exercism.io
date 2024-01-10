RegExp UPPER = RegExp(r'\p{Uppercase}', unicode: true);
RegExp LOWER = RegExp(r'\p{Lowercase}', unicode: true);

class Bob {
  String response(String input) {
    var trimmed = input.trimRight();
    var result = (trimmed.endsWith('?') ? 1 : 0) +
        (trimmed.contains(UPPER) && !trimmed.contains(LOWER) ? 2 : 0) +
        (trimmed.length == 0 ? 4 : 0);

    switch (result) {
      case 4:
        return 'Fine. Be that way!';
      case 3:
        return "Calm down, I know what I'm doing!";
      case 2:
        return 'Whoa, chill out!';
      case 1:
        return 'Sure.';
      default:
        return 'Whatever.';
    }
  }
}
