const MINUTES = 60;
const HOURS = 24;

export class Clock {
  constructor(hour = 0, minute = 0) {
    if (!Number.isInteger(hour) || !Number.isInteger(minute)) {
      throw new Error('invalid argument');
    }
    this.minutes = hour * MINUTES + minute;
  }

  plus(minutes) {
    this.minutes += minutes;
    return this;
  }

  minus(minutes) {
    return this.plus(-minutes);
  }

  normalize() {
    const round = this.minutes < 0 ? Math.ceil : Math.floor; // round toward zero
    let [h, m] = [round(this.minutes / MINUTES), this.minutes % MINUTES];
    // wrap the minutes
    if (m < 0) {
      h -= 1;
      m += MINUTES;
    }
    // wrap the hours
    h = ((h % HOURS) + HOURS) % HOURS;
    return [h, m];
  }

  toString() {
    return this.normalize().map(n => n.toString().padStart(2, '0')).join(':');
  }

  equals(aClock) {
    return this.toString() === aClock.toString();
  }
}
