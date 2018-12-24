/* eslint-disable class-methods-use-this, no-multi-spaces */
/* eslint no-underscore-dangle: ["error", { "allowAfterThis": true }] */

const randomInt = max => Math.floor(Math.random() * Math.floor(max));
const rLetter   = ()  => 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'[randomInt(26)];
const rDigit    = ()  => randomInt(10);

const allNames = new Set();

class Robot {
  constructor() {
    this.reset();
  }

  reset() {
    let name;
    do {
      name = rLetter() + rLetter() + rDigit() + rDigit() + rDigit();
    } while (allNames.has(name));
    this._name = name;
    allNames.add(name);
  }

  // getter,setter to handle "name" as a property.
  get name() {
    return this._name;
  }

  set name(str) {
    throw new Error('names cannot be modified');
  }
}

module.exports = Robot;
