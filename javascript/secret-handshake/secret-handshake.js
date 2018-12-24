/* eslint-disable no-bitwise */

const shakes = [
  'wink',
  'double blink',
  'close your eyes',
  'jump',
];

const secretHandshake = (n) => {
  if (/\D/.test(n)) throw new Error('Handshake must be a number');
  const actions = shakes.filter((s, i) => n & (1 << i));
  return (n & (1 << shakes.length)) ? actions.reverse() : actions;
};

module.exports = { secretHandshake };

/* community
 *

class SecretHandshake {
  constructor(number) {
    if (typeof number !== "number") throw Error("Handshake must be a number");
    this.cmds = [];
    if ((number & 0b00001) === 0b00001) this.cmds.push("wink");
    if ((number & 0b00010) === 0b00010) this.cmds.push("double blink");
    if ((number & 0b00100) === 0b00100) this.cmds.push("close your eyes");
    if ((number & 0b01000) === 0b01000) this.cmds.push("jump");
    if ((number & 0b10000) === 0b10000) this.cmds.reverse();
  }

  commands() {
    return this.cmds;
  }
}


*/
