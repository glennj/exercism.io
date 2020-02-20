class PhoneNumber(input: String) {
    val number = input.filter(Char::isDigit)
    init { require(number.matches(Regex("^1?([2-9]\\d{2}){2}\\d{4}$"))) }
}
