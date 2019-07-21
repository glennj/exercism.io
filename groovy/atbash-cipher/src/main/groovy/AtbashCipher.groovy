class AtbashCipher {

    static final ALPHABET = ('a'..'z').join('')
    static final TEBAHPLA = ALPHABET.reverse()

    static String encode(phrase) {
        words(decode(phrase), 5)
    }

    static String words(phrase, length) {
        phrase.toList().collate(length)*.join('').join(" ")
    }

    static String decode(phrase) {
        phrase.toLowerCase()
              .replaceAll('[^a-z0-9]', '')
              .tr(ALPHABET, TEBAHPLA)
    }

}
