class SecretHandshake {

    static final ACTIONS = ['wink', 'double blink', 'close your eyes', 'jump']
    static final REVERSE = ACTIONS.size()

    static List<String> commands(int number) {
        def handshake = []
        ACTIONS.eachWithIndex { a, i ->
            if ((number & (1 << i)) != 0) handshake << a
        }
        (number & (1 << REVERSE)) != 0
            ? handshake.reverse()
            : handshake
    }
}
