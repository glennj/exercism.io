/* eslint-disable no-bitwise */

const shakes = [
  'wink',
  'double blink',
  'close your eyes',
  'jump',
];

export const commands = (n) => {
  if (/\D/.test(n)) throw new Error('Handshake must be a number');
  const actions = shakes.filter((s, i) => n & (1 << i));
  return (n & (1 << shakes.length)) ? actions.reverse() : actions;
};
