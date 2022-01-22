"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.chain = void 0;

const chain = dominoes => {
  if (dominoes.length == 0) return [];
  return dominoes.forEach((d, i) => {
    var remaining = dominoes.filter((_, idx) => idx != i);
    var chain = buildChain(d, remaining);

    if (chain.length == 0) {
      chain = buildChain(reverse(d), remaining);
    }

    if (chain.length != 0) {
      return chain;
    }
  });
};

exports.chain = chain;

const reverse = domino => {
  return [domino[1], domino[0]];
};

const buildChain = (chain, available) => {
  var tail = chain[chain.length - 1][1];

  if (available.length == 0) {
    var head = chain[0][0];
    if (head == tail) return chain;else return [];
    var theChain = [];
    available.forEach((d, i) => {
      if (d.includes(tail)) {
        if (d[1] != tail) d = reverse(d);
        var newChain = buildChain([...chain, d], available.filter((_, idx) => idx != i));
        if (newChain.length != 0) return newChain;
      }
    });
    return theChain;
  }
};

