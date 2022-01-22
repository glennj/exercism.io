/// <reference path="./global.d.ts" />
// @ts-check

/**
 * Implement the functions needed to solve the exercise here.
 * Do not forget to export them so they are available for the
 * tests. Here an example of the syntax as reminder:
 *
 * export function yourFunction(...) {
 *   ...
 * }
 */



export function cookingStatus(remainingTime) {
  if (remainingTime === null || remainingTime === undefined)
    return "You forgot to set the timer.";

  if (remainingTime == 0)
    return "Lasagna is done.";

  return "Not done, please wait.";
}


export function preparationTime(layers, avgTime) {
  avgTime ??= 2;
  return layers.length * avgTime;
}


export function quantities(layers) {
  const amt = {"noodles": 50, "sauce": 0.2};
  const qty = {"noodles":  0, "sauce": 0.0};
  layers.forEach((item) => {if (item in qty) qty[item] += amt[item]});
  return qty;
}


export function addSecretIngredient(friendsList, myList) {
  // version mismatch
  // const secret = friendsList.at(-1);
  const secret = friendsList[friendsList.length-1];
  myList.push(secret);
  return;
}


export function scaleRecipe(original, portions) {
  const scale = portions / 2;
  const scaled = {};
  for (const item in original) {
    scaled[item] = scale * original[item];
  }
  return scaled;
}
