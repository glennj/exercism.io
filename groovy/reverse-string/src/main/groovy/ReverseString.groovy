class ReverseString {

    static reverse(String value) {
        // the builtin solution
        /*
        value.reverse()
        */

        // split-reverse-join
        /*
        value.toList().reverse().join("")
        */

        // manually reversing
        def rev = new StringBuilder()
        value.toList().each { rev[0..<0] = it }
        rev.toString()
    }
}
