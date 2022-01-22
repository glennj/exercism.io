export class Bowling {
  constructor() {
    this.currentFrame = [];
    this.frames = [];
  }

  get is10thFrame() { return this.frames.length === 9; }

  get isGameOver() { return this.frames.length === 10; }

  roll(pins) {
    if (this.isGameOver) throw new Error('Cannot roll after game is over');
    if (pins < 0) throw new Error('Negative roll is invalid');
    if (pins > 10) throw new Error('Pin count exceeds pins on the lane');

    switch (this.currentFrame.length) {
      case 0: this.roll1(pins); break;
      case 1: this.roll2(pins); break;
      case 2: this.roll3(pins); break;
      default: throw new Error('Too many rolls.');
    }
  }

  roll1(first) {
    if (!this.is10thFrame && first === 10) {
      this.frames.push([10]);
    } else {
      this.currentFrame.push(first);
    }
  }

  roll2(second) {
    const [first] = this.currentFrame;
    if (first < 10 && first + second > 10) {
      throw new Error('Pin count exceeds pins on the lane');
    }
    if (!this.is10thFrame || first + second < 10) {
      this.frames.push([this.currentFrame.shift(), second]);
    } else {
      this.currentFrame.push(second);
    }
  }

  roll3(third) {
    const [first, second] = this.currentFrame;
    if (first === 10 && second < 10 && second + third > 10) {
      throw new Error('Pin count exceeds pins on the lane');
    }
    this.frames.push([first, second, third]);
  }

  score() {
    if (!this.isGameOver) {
      throw new Error('Score cannot be taken until the end of the game');
    }
    const rolls = this.frames.reduce((flat, list) => flat.concat(list), []);
    let score = 0;
    for (let frame = 1; rolls.length > 0; frame += 1) {
      if (frame === 10) {
        score += rolls.splice(0).reduce((sum, roll) => sum + roll, 0);
      } else if (rolls[0] === 10) {
        score += rolls.shift() + rolls[0] + rolls[1];
      } else if (rolls[0] + rolls[1] === 10) {
        score += rolls.shift() + rolls.shift() + rolls[0];
      } else {
        score += rolls.shift() + rolls.shift();
      }
    }
    return score;
  }
}
