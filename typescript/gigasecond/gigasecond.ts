const gigasecond = 1e9
const giga_milliseconds = gigasecond * 1e3

class Gigasecond {

  readonly gigadate: Date

  constructor(date: Date) {
    this.gigadate = new Date(date.valueOf() + giga_milliseconds)
  }

  date(): Date { return this.gigadate }
}

export default Gigasecond
