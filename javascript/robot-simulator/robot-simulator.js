const bearings = [];
bearings[0] = 'east';
bearings[90] = 'north';
bearings[180] = 'west';
bearings[270] = 'south';

const instructions = {
  A: 'advance',
  R: 'turnRight',
  L: 'turnLeft',
};

export class InvalidInputError extends Error {}

export class Robot {
  #coordinates;
  #bearing;

  constructor() {
    this.#coordinates = [0, 0];
    this.#bearing = 90;
  }

  get bearing() { return bearings[this.#bearing]; }

  get coordinates() { return this.#coordinates; }

  at(x, y) {
    this.#coordinates = [x, y];
    return this;
  }

  orient(bearing) {
    if (!bearings.includes(bearing)) throw new InvalidInputError(`invalid orientation ${bearing}`);
    this.#bearing = bearings.indexOf(bearing);
    return this;
  }

  place({ x, y, direction }) {
    return this.at(x, y).orient(direction);
  }

  turn(amount) {
    this.#bearing = (this.#bearing + amount + 360) % 360;
    return this;
  }

  turnLeft() { this.turn(+90); }

  turnRight() { this.turn(-90); }

  advance() {
    const rad = this.#bearing * 2 * Math.PI / 360;
    const [x, y] = this.#coordinates;
    const [dx, dy] = [Math.round(Math.cos(rad)), Math.round(Math.sin(rad))];
    return this.at(x + dx, y + dy);
  }

  static instructions(script) {
    return Array.from(script).map((i) => {
      if (!instructions[i]) throw new InvalidInputError(`no such instruction ${i}`);
      return instructions[i];
    });
  }

  evaluate(script) {
    Robot.instructions(script).forEach(i => this[i]());
  }
}
