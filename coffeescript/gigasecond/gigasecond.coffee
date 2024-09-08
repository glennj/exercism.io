class Gigasecond
  BillionMilliseconds = 10**9 * 10**3
  
  @add: (moment) -> new Date(moment.valueOf() + BillionMilliseconds)

module.exports = Gigasecond
