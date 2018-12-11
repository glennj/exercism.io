const hanoi = (n, from='A', to='C', using='B') => {
  if (n === 1) {
    console.log(`move from ${from} to ${to}`);
    return 1;
  } else {
    return hanoi(n-1, from,  using, to)
         + hanoi(  1, from,  to)
         + hanoi(n-1, using, to, from);
  }
};

n = 4;
console.log(`expected to take ${Math.pow(2, n) - 1} moves`);
numMoves = hanoi(n);
console.log(`used ${numMoves} moves`);
