export const encode = (str) => {
  if (/\d/.test(str)) throw new Error('cannot encode digits');
  if (str === '') return '';
  const words = str.match(/((.)\2*)/g);
  return words.reduce((result, sequence) => {
    const len = sequence.length === 1 ? '' : sequence.length;
    return result + len + sequence[0];
  }, '');
};

export const decode = (str) => {
  if (str === '') return '';
  const sequences = str.match(/(\d*\D)/g);
  return sequences.reduce((result, pair) => {
    const [len, c] = pair.match(/(\d*)(\D)/).slice(1, 3);
    return result + c.repeat(len || 1);
  }, '');
};
