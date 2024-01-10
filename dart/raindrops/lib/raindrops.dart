class Raindrops {
  String convert(int num) {
    var drops = StringBuffer();
    if (num % 3 == 0) drops.write('Pling');
    if (num % 5 == 0) drops.write('Plang');
    if (num % 7 == 0) drops.write('Plong');
    if (drops.isEmpty) drops.write(num);
    return drops.toString();
  }
}
