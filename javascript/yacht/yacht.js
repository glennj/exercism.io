
function score(dice, category) {
  let score = 0;
  const groups = groupDice(dice);
  const counts = Object.values(groups);

  switch (category) {
    case 'ones':
      score = scoreFor(groups, 1);
      break;
    case 'twos':
      score = scoreFor(groups, 2);
      break;
    case 'threes':
      score = scoreFor(groups, 3);
      break;
    case 'fours':
      score = scoreFor(groups, 4);
      break;
    case 'fives':
      score = scoreFor(groups, 5);
      break;
    case 'sixes':
      score = scoreFor(groups, 6);
      break;
    case 'choice':
      score = dice.sum();
      break;
    case 'yacht':
      if (counts.length == 1) {
        score = 50;
      }
      break;
    case 'full house':
      if (counts.length == 2 && counts.includes(3)) {
        score = dice.sum();
      }
      break;
    case 'four of a kind':
      // can score yacht as four of a kind
      if (counts.length <= 2) {
        for (const die in groups) {
          if (groups[die] >= 4) {
            score = 4 * die;
            break;
          }
        }
      }
      break;
    case 'little straight':
      if (dice.sort().join('') === '12345') {
        score = 30;
      }
      break;
    case 'big straight':
      if (dice.sort().join('') === '23456') {
        score = 30;
      }
      break;
  }
  return score;
}

Array.prototype.sum = function() {
  return this.reduce((elem, sum) => elem + sum);
};

function groupDice(dice) {
  const groups = {};
  for (const die of dice) {
    groups[die] = (groups[die] || 0) + 1;
  }
  return groups;
}

function scoreFor(groups, die) {
  return die * (groups[die] || 0);
}

export { score };
