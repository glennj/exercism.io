export const getItem = (cards, position) => cards[position];

export const setItem = function(cards, position, replacementCard) {
  cards[position] = replacementCard;
  return cards;
};

export const insertItemAtTop = function(cards, newCard) {
  cards.push(newCard);
  return cards;
};

export const removeItem = function(cards, position) {
  cards.splice(position, 1);
  return cards;
};

export const removeItemFromTop = function(cards) {
  cards.pop();
  return cards;
};

export const insertItemAtBottom = function(cards, newCard) {
  cards.unshift(newCard);
  return cards;
};

export const removeItemAtBottom = function(cards) {
  cards.shift();
  return cards;
};

export const checkSizeOfStack = (cards, stackSize) => cards.length === stackSize;
