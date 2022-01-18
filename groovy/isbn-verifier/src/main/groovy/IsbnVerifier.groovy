class IsbnVerifier {
    static boolean isValid(String isbn) {
        isbn = isbn.replaceAll("-", "")
        if (!isbn.find('^\\d{9}[\\dX]$')) return false

        def sum = 0
        isbn[0..8].toList()*.toInteger().eachWithIndex { 
            x, i -> sum += x * (10 - i) 
        }
        sum += isbn[9] == "X" ? 10 : isbn[9].toInteger()
        sum % 11 == 0

    }
}
