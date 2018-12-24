function isArmstrongNumber(n: number): boolean {
  const len = Math.ceil( Math.log10( n ))
  let armstrongSum = 0
  let tmp = n
  while (tmp > 0) {
    const digit = tmp % 10
    armstrongSum += digit ** len
    tmp = (tmp - digit) / 10
  }
  return armstrongSum === n
}

export default { isArmstrongNumber }
