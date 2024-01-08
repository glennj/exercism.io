import 'forth_stack.dart';
import 'forth_words.dart';

class Forth {
  ForthStack _stack = ForthStack();
  ForthWords _words = ForthWords();

  List<int> get stack => _stack.toList();

  void evaluate(String program) => program.split('\n')
                                          .map((s) => s.toUpperCase())
                                          .forEach(_evaluateLine);

  void _evaluateLine(line) {
    var tokens = line.split(' ');
    while (tokens.isNotEmpty) {
      var token = tokens.removeAt(0);
      var number = int.tryParse(token);
      if (number != null) _stack.push(number);
      else if (_words.hasDefinition(token)) tokens = _words.get(token) + tokens;
      else if (token == '+') _stack.add();
      else if (token == '-') _stack.subtract();
      else if (token == '*') _stack.multiply();
      else if (token == '/') _stack.divide();
      else if (token == 'DUP') _stack.dup();
      else if (token == 'DROP') _stack.drop();
      else if (token == 'SWAP') _stack.swap();
      else if (token == 'OVER') _stack.over();
      else if (token == ':') {
        _words.add(tokens);
        tokens = [];
      }
      else throw Exception('Unknown command');
    }
  }
}
