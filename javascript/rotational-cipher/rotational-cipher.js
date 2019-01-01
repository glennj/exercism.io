const alphabet = 'abcdefghijklmnopqrstuvwxyz';

const encipher = (c, n) => {
  const i = alphabet.indexOf(c.toLowerCase());
  if (i === -1) return c;
  const d = alphabet[(i + n) % alphabet.length];
  return (/[A-Z]/.test(c) ? d.toUpperCase() : d);
};

const rotate = (str, n = 13) => str.split('').map(c => encipher(c, n)).join('');

module.exports = { rotate };
