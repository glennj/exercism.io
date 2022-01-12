export const score = (dice, category) => {
  const groups = [-1, 0,0,0,0,0,0];       // dummy value for 0th index
  dice.forEach(die => groups[die] += 1);

  const counts   = groups.filter(c => c > 0);
  const sum      = dice.reduce((sum, die) => sum + die);
  const scoreFor = (die) => groups[die] * die;

  return {
    'ones':   () => scoreFor(1),
    'twos':   () => scoreFor(2),
    'threes': () => scoreFor(3),
    'fours':  () => scoreFor(4),
    'fives':  () => scoreFor(5),
    'sixes':  () => scoreFor(6),
    'full house':      () => counts.length == 2 && counts.includes(3) ? sum : 0,
    'four of a kind':  () => groups.reduce((score, count, die) => count >= 4 ? 4 * die : score, 0),
    'little straight': () => dice.sort().join('') === '12345' ? 30 : 0,
    'big straight':    () => dice.sort().join('') === '23456' ? 30 : 0,
    'yacht':  () => counts.length == 1 ? 50 : 0,
    'choice': () => sum,
  }[category]();
};
