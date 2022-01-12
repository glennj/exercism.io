export const compute = (a, b) => {
  if (a.length !== b.length) {
    throw new Error('strands must be of equal length');
  }

  // straightforward solution, loop over the indices
  /*
  let distance = 0;
  for (let i = 0; i < a.length; i += 1) {
    if (a[i] !== b[i]) {
      distance += 1;
    }
  }
  return distance;
  */

  // functional solution
  return a
    .split('')
    .reduce((ham, c, i) => ham + (c === b[i] ? 0 : 1), 0);
};
