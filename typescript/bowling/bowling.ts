class Bowling {
  private rolls: number[]
  constructor(rolls: number[]) {
    this.rolls = rolls
  }

  score(): number {
    let score = 0

    // first 9 frames
    for (let frame = 1; frame < 10; frame++) {
      const [ball1, ball2] = this.getFrame(frame)
      score += ball1 + ball2
      if (ball1 + ball2 === 10) {
        this.expect(1)
        score += this.rolls[0]
      }
      if (ball1 === 10) {
        this.expect(2)
        score += this.rolls[1]
      }
    }

    // 10th frame
    score += this.getFrame(10).reduce((sum, ball) => sum + ball)

    if (this.rolls.length > 0) {
      throw new Error('Should not be able to roll after game is over')
    }
    return score
  }

  getFrame(frame: number): number[] {
    const ball1 = this.getBall()
    let ball2 = 0
    let ball3 = 0

    if (ball1 === 10 && frame < 10) {
      return [ball1, ball2]
    }
    ball2 = this.getBall()
    if (ball1 < 10 && ball1 + ball2 > 10) {
      throw new Error('Pin count exceeds pins on the lane')
    }
    if (frame < 10) {
      return [ball1, ball2]
    }

    // 10th frame
    if (ball1 === 10 || ball1 + ball2 === 10) {
      ball3 = this.getBall()
      if (ball1 === 10 && ball2 < 10 && ball2 + ball3 > 10) {
        throw new Error('Pin count exceeds pins on the lane')
      }
    }
    return [ball1, ball2, ball3]
  }

  private getBall(): number {
    this.expect(1)
    const ball = this.rolls.shift() || 0
    if (ball < 0 || ball > 10) {
      throw new Error('Pins must have a value from 0 to 10')
    }
    return ball
  }

  private expect(n: number): void {
    if (this.rolls.length < n) {
      throw new Error('Score cannot be taken until the end of the game')
    }
  }
}

export default Bowling
