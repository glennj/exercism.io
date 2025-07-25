export const recite = (initialBottlesCount, takeDownCount) => {
  return [...Array(takeDownCount).keys()]
    .map(i => initialBottlesCount - i)
    .reduce((xs, n) => [...xs, '', ...verse(n)], [])
    .slice(1);
};

const verse = (n) => [
  `${bottles(n)} hanging on the wall,`,
  `${bottles(n)} hanging on the wall,`,
  'And if one green bottle should accidentally fall,',
  `There'll be ${bottles(n-1).toLowerCase()} hanging on the wall.`
];

const NUMBER = [
  'No', 'One', 'Two', 'Three', 'Four', 'Five',
  'Six', 'Seven', 'Eight', 'Nine', 'Ten'
];

const bottles = (n) => `${NUMBER[n]} green bottle${n == 1 ? "" : "s"}`;
