class PerfectNumbers {

    static Classification classify(int num) {
        if (num <= 0) throw new ArithmeticException()

        def sum = aliquotSum(num)

        if (sum < num) return Classification.DEFICIENT
        if (sum > num) return Classification.ABUNDANT
        return Classification.PERFECT
    }

    static int aliquotSum(int num) {
        def factors = new HashSet()
        1.upto(Math.floor(Math.sqrt(num))) {
            if (num % it == 0) {
                factors.addAll([it, num.intdiv(it)])
            }
        }
        factors.removeElement(num)
        return factors.sum() ?: 0
    }
}
