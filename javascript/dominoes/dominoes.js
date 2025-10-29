"use strict";

export const chain = (dominoes) => {
  if (dominoes.length === 0) return [];

  for (var i = 0; i < dominoes.length; i++) {
    var d = dominoes[i];
    var remaining = dominoes.filter((_, idx) => idx !== i);
    var ch = buildChain([d], remaining);

    if (!ch) ch = buildChain([reverse(d)], remaining);

    if (ch) return ch;
  }

  return null;
};

const reverse = (domino) => [domino[1], domino[0]];

const buildChain = (current, available) => {
  var tail = current[current.length - 1][1];

  if (available.length === 0) {
    var head = current[0][0];
    return head === tail ? current : null;
  }

  for (var i = 0; i < available.length; i++) {
    var d = available[i];
    if (d.includes(tail)) {
      if (d[0] !== tail) d = reverse(d);
      var rest = available.filter((_, idx) => idx !== i);
      var newChain = buildChain([...current, d], rest);
      if (newChain) return newChain;
    }
  }
  return null;
};

