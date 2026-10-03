import 'dart:math';

typedef Result = ({int moves, String goalBucket, int otherBucket});

// ----------------------------------------------------------
class Bucket {
  Bucket(this.name, this.size);

  String name;
  int size;
  int amount = 0;

  bool isFull() => amount == size;
  bool isEmpty() => amount == 0;
  void fill() => amount = size;
  void empty() => amount = 0;
  int availability() => size - amount;
  void pourInto(Bucket other) {
    var quantity = min(amount, other.availability());
    amount -= quantity;
    other.amount += quantity;
  }

  @override
  String toString() => "Bucket($name, $size, $amount)";
}

// ----------------------------------------------------------
class TwoBucket {
  late Bucket first;
  late Bucket second;
  late int goal;
  bool valid = true;
  String err = '';

  TwoBucket(
      {required int bucketOne,
      required int bucketTwo,
      required int goal,
      required String startBucket}) {
    this.goal = goal;
    if (startBucket == "one") {
      first = Bucket("one", bucketOne);
      second = Bucket("two", bucketTwo);
    } else {
      first = Bucket("two", bucketTwo);
      second = Bucket("one", bucketOne);
    }

    // pre-validation
    if (goal > max(first.size, second.size)) {
      valid = false;
      err = 'Measurement is impossible: goal too big';
    }
    var gcd = first.size.gcd(second.size);
    if (gcd != 1 && goal % gcd != 0) {
      valid = false;
      err = 'Measurement is impossible: goal is unsatisfiable';
    }
  }

  Result measure() {
    if (!valid) throw ArgumentError(err);

    first.fill();
    second.empty();
    var moves = 1;

    if (goal == second.size && goal != first.size) {
      second.fill();
      moves++;
    }

    while (true) {
      if (goal == first.amount) return _result(moves, first, second);
      if (goal == second.amount) return _result(moves, second, first);

      // ignore_for_file: curly_braces_in_flow_control_structures
      if (first.isEmpty())       first.fill();
      else if (second.isFull())  second.empty();
      else                       first.pourInto(second);

      moves++;
    }
  }

  Result _result(int moves, Bucket winner, Bucket loser) {
    return (moves: moves, goalBucket: winner.name, otherBucket: loser.amount);
  }
}
