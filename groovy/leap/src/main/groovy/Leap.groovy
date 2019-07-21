class Leap {

    int year

    Leap(Integer year) {
        this.year = year
    }

    def isLeapYear() {
        this.year % 400 == 0 || ( 
            this.year % 4 == 0 && this.year % 100 != 0 
        )
    }
}
