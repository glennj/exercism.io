class Isogram {
    static boolean isIsogram(String phrase) {
        phrase.toLowerCase()
              .findAll("\\p{Alpha}")
              .groupBy { it }
              .every { it.value.size() == 1 }
    }
}
