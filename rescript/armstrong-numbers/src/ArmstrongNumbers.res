let isArmstrongNumber = num => {
  let width = 1 + num->Int.toFloat->Math.log10->Float.toInt
  let rec armstrongSum = (n, sum) => {
    switch (n / 10, n % 10) {
    | (0, 0) => sum
    | (quo, rem) => armstrongSum(quo, sum + rem ** width)
    }
  }
  num === armstrongSum(num, 0)
}
