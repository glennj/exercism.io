// @ts-check
//
// The line above enables type checking for this file. Various IDEs interpret
// the @ts-check directive. It will give you helpful autocompletion when
// implementing this exercise.

/**
 * Calculates the total bird count.
 *
 * @param {number[]} birdsPerDay
 * @returns {number} total bird count
 */
export function totalBirdCount(birdsPerDay) {
  /*
  let sum = 0;
  for (let i = 0; i < birdsPerDay.length; i++) 
    sum += birdsPerDay[i];
  return sum;
  */
  return birdsPerDay.reduce((sum, num) => sum + num, 0);
}

/**
 * Calculates the total number of birds seen in a specific week.
 *
 * @param {number[]} birdsPerDay
 * @param {number} week
 * @returns {number} birds counted in the given week
 */
export function birdsInWeek(birdsPerDay, week) {
  let weekNum = 0;
  while (birdsPerDay.length > 0) {
    let thisWeek = birdsPerDay.slice(7 * weekNum, 7 * (weekNum + 1));
    console.log([weekNum, thisWeek]);
    if (++weekNum == week) 
      return totalBirdCount(thisWeek);
  };
  return;
}

/**
 * Fixes the counting mistake by increasing the bird count
 * by one for every second day.
 *
 * @param {number[]} birdsPerDay
 * @returns {number[]} corrected bird count data
 */
export function fixBirdCountLog(birdsPerDay) {
  birdsPerDay.forEach((num, idx) => birdsPerDay[idx] = num + (idx % 2 == 0));
  return birdsPerDay;
}