export const compute = (a, b) => {
  if (a.length !== b.length) {
    throw new Error('left and right strands must be of equal length');
  }
  let distance = 0;
  for (let i = 0; i < a.length; i += 1) {
    if (a[i] !== b[i]) {
      distance += 1;
    }
  }
  return distance;
};

/* community
 *
  export default () => ({
      compute: (strand1 = '', strand2 = '') => {
        if (strand1.length !== strand2.length) throw new Error("DNA strands must be of equal length.");
        return [...strand1].filter((nuc, pos) => nuc !== strand2.charAt(pos)).length;
      }
    }); 
