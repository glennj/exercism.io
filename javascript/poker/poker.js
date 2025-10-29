import { PokerHand } from './pokerHand';

export const bestHands = (hands) => {
  var hs = hands
      .map((h) => new PokerHand(h))
      .toSorted(PokerHand.compare);

  var [maxType, maxRanking] = [hs[0].type, hs[0].ranking];

  return hs
      .filter((h) => h.type === maxType && h.ranking === maxRanking)
      .map((h) => h.toString());
};
