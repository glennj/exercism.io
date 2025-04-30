// monkey patching!
String.prototype.take = function (n) {
  return this.slice(0, n);
};
String.prototype.drop = function (n) {
  return this.slice(n);
};
String.prototype.takeEnd = function (n) {
  return this.slice(-n);
};
String.prototype.toCapitalized = function () {
  return this.take(1).toUpperCase() + this.drop(1).toLowerCase();
}

export const frontDoorResponse = (word) => word.trimStart().take(1);;
export const frontDoorPassword = (word) => word.toCapitalized();

export const backDoorResponse = (word) => word.trimEnd().takeEnd(1);
export const backDoorPassword = (word) => word.toCapitalized() + ', please';
