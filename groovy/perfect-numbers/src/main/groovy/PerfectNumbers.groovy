class PerfectNumbers {

    static Classification classify(int num) {
        if (num <= 0) throw new ArithmeticException()
        if (num <= 2) return Classification.DEFICIENT

        def sum = aliquotSum(num)

        if (sum < num) return Classification.DEFICIENT
        if (sum > num) return Classification.ABUNDANT
        return Classification.PERFECT
    }

    static int aliquotSum(int num) {
        def sum = 1
        2.upto(Math.floor(Math.sqrt(num))) {
            if (num % it == 0) {
                def q = num.intdiv(it)
                sum += it + (it == q ? 0 : q)
            }
        }
        sum
    }
}
