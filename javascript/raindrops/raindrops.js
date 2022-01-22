export const convert = (n) => {
  return [[3, 'Pling'], [5, 'Plang'], [7, 'Plong']]
    .reduce((rain, [f, s]) => rain + (n % f === 0 ? s : ''), '')
      || n.toString();
};
