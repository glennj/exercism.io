class Grains {
    static square(num) {
        //1 << (num - 1) // wraps at 2^32
        2 ** (num - 1)
    }

    static total() {
        //(1..64).collect {square(it)}.sum()
        (2 ** 64) - 1
    }
}
