import 'package:two_bucket/two_bucket.dart';

void main(List<String> args) {
  var b1 = Bucket("one", 3);
  var b2 = Bucket("two", 5);
  print([b1.toString(), b2.toString()]);
  b1.fill();
  print([b1.toString(), b2.toString()]);
  b1.empty();
  print([b1.toString(), b2.toString()]);
}
