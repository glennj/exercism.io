class RunLengthEncoder {
  String encode(String text) {
    return text.replaceAllMapped(
        RegExp(r'(.)\1+'), (m) => "${m[0]!.length}${m[1]!}");
  }

  String decode(String text) {
    return text.replaceAllMapped(
        RegExp(r'(\d+)(\D)'), (m) => m[2]! * int.parse(m[1]!));
    return "no";
  }
}
