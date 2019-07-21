class ArmstrongNumber {

    static isArmstrongNumber(number) {
        def n = number
        if (n < 0) throw new IllegalArgumentException()

        /*  
        def digits = []
        while (n > 0) {
            digits << n.mod(10)
            n = n.intdiv(10)
        }
        def sum = digits*.power(digits.size()).sum() ?: 0
        */

        def sum = 0
        def len = Math.ceil(Math.log10(number))

        while (n > 0) {
            sum += (n % 10) ** len
            n = n.intdiv(10)
        }
        
        number == sum
    }
}
