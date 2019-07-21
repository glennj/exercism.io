class RotationalCipher {
    private String alphabet
    private String rotated

    RotationalCipher(int key) {
        alphabet = ('a'..'z').join('')
        rotated  = alphabet.drop(key) + alphabet.take(key)

        alphabet += alphabet.toUpperCase()
        rotated  += rotated.toUpperCase()
    }

    String rotate(String cipherText) {
        cipherText.tr(alphabet, rotated)
    }
}
