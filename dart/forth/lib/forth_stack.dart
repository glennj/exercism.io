class ForthStack {
  List<int> _stack = [];

  void dispatch(String word) {
    switch (word) {
      case '+': add();
      case '-': subtract();
      case '*': multiply();
      case '/': divide();
      case 'DUP': dup();
      case 'DROP': drop();
      case 'SWAP': swap();
      case 'OVER': over();
      default: throw Exception('Unknown command');
    }
  }

  int get size => _stack.length;

  List<int> toList() => List<int>.from(_stack);

  void push(int n) => _stack.add(n);
  int pop() => _stack.removeLast();
  int peek() => _stack.last;

  void _needs(int n) {
    if (size < n) throw Exception('Stack empty');
  }

  void dup() {
    _needs(1);
    push(peek());
  }

  void drop() {
    _needs(1);
    pop();
  }

  void swap() {
    _needs(2);
    var b = pop();
    var a = pop();
    push(b);
    push(a);
  }

  void over() {
    _needs(2);
    var b = pop();
    var a = peek();
    push(b);
    push(a);
  }

  void add() {
    _needs(2);
    var b = pop();
    var a = pop();
    push(a + b);
  }

  void subtract() {
    _needs(2);
    var b = pop();
    var a = pop();
    push(a - b);
  }

  void multiply() {
    _needs(2);
    var b = pop();
    var a = pop();
    push(a * b);
  }

  void divide() {
    _needs(2);
    var b = pop();
    if (b == 0) throw Exception('Division by zero');
    var a = pop();
    push(a ~/ b);
  }
}
