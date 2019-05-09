struct Year {
    var isLeapYear: Bool
    
    init(calendarYear y: Int) {
        isLeapYear = y % 400 == 0 || (y % 100 != 0 && y % 4 == 0)
    }
}
