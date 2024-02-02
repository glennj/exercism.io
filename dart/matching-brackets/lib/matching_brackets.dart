// map the close bracket to it's opening companion
const BRACKETS = <String,String>{
  ')': '(',
  ']': '[',
  '}': '{'
};

extension on String {
  bool get isOpenBracket => BRACKETS.values.contains(this);
  bool get isCloseBracket => BRACKETS.keys.contains(this);
  String openBracket() => BRACKETS[this] ?? '';
}

extension on List {
  void push<E>(E elem) => this.add(elem);
  E pop<E>() => this.removeLast();
}


class MatchingBrackets {
  bool isPaired(String input) {
    var stack = <String>[];

    for (var char in input.split('')) {
      if (char.isOpenBracket) {
        stack.push(char);
      }
      else if (char.isCloseBracket) {
        if (stack.isEmpty || char.openBracket() != stack.pop()) {
          return false;
        }
      }
    }
    return stack.isEmpty;
  }
}
