/* eslint-disable  no-multi-spaces, default-case  */
/* eslint no-underscore-dangle: ["error", { "allowAfterThis": true }] */

export class InvalidInputError extends Error {}

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

export default class Robot {
  constructor() {
    this._coordinates = [0, 0];
    this._bearing = 0;
  }

  get bearing() { return bearings[this._bearing]; }

  get coordinates() { return this._coordinates; }

  at(x, y) {
    if (!Number.isInteger(x)) throw new InvalidInputError(`not an int: ${x}`);
    if (!Number.isInteger(y)) throw new InvalidInputError(`not an int: ${y}`);
    this._coordinates = [x, y];
    return this;
  }

  orient(bearing) {
    if (!bearings.includes(bearing)) throw new InvalidInputError(`invalid orientation ${bearing}`);
    this._bearing = bearings.indexOf(bearing);
    return this;
  }

  place({ x, y, direction }) {
    return this.at(x, y).orient(direction);
  }

  turn(amount) {
    this._bearing = (this._bearing + amount + 360) % 360;
    return this;
  }

  turnLeft() { this.turn(+90); }

  turnRight() { this.turn(-90); }

  advance() {
    const rad = this._bearing * 2 * Math.PI / 360;
    const [x, y] = this._coordinates;
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
