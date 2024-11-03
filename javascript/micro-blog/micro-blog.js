export const truncate = (input, len = 5) => input.characters().take(len).join('');

// ref: https://stackoverflow.com/a/59796758/7552
String.prototype.characters = function () {return [...this]};

Array.prototype.take = function (n) {return this.slice(0, n)};
