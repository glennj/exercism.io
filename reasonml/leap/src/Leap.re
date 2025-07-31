
let isLeapYear = (year) => {
    let divBy = (num: int, den: int): bool => num mod den == 0;

    divBy(year, 4) && (!divBy(year, 100) || divBy(year, 400));
}
