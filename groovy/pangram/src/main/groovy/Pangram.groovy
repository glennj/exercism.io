class Pangram {

    static boolean isPangram(String sentence) {
        def letters = sentence.toUpperCase()
                              .replaceAll('[^A-Z]', '')
                              .toSet()
        letters.size() == ('A'..'Z').size()
    }

}
