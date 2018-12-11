class Clock {
  private hour: number
  private minute: number

  constructor(h: number, m?: number) {
    this.hour = h || 0
    this.minute = m || 0
    this.normalize()
  }

  private normalize(): void {
    if (this.minute < 0 || this.minute >= 60) {
      this.hour += Math.floor(this.minute / 60)
      this.minute = this.positiveMod(this.minute, 60)
    }
    this.hour = this.positiveMod(this.hour, 24)
  }

  // n % d < 0 if n < 0
  private positiveMod(n: number, d: number): number {
    return (n % d + d) % d
  }

  toString(): string {
    return String(this.hour).padStart(2, '0')
         + ':'
         + String(this.minute).padStart(2, '0')
  }

  equals(other: Clock): boolean {
    return this.toString() === other.toString()
  }

  plus(minutes: number): Clock {
    return new Clock(this.hour, this.minute + minutes)
  }

  minus(minutes: number): Clock {
    return this.plus(-minutes)
  }
}

export default Clock
