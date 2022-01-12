class PhoneNumber(input: String) {
    val filtered = input.filter(Char::isDigit)  // removes non-digits

    init {
      require(filtered.matches(Regex("^1?(?:[2-9]\\d{2}){2}\\d{4}$")))
    }

    val number = filtered.trimStart('1')
}
