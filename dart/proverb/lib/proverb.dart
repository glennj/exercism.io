class Proverb {
  String recite(List<String> items) {
    var proverb = StringBuffer();
    for (var i = 0; i < items.length - 1; i++)
      proverb.write("For want of a ${items[i]} the ${items[i + 1]} was lost.\n");
    if (items.isNotEmpty)
      proverb.write("And all for the want of a ${items[0]}.");
    return proverb.toString();
  }
}
