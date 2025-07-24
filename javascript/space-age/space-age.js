/* eslint-disable no-new-func */

const secondsPerEarthYear = 31557600;

const planetaryYears = {
  mercury: 0.2408467,
  venus: 0.61519726,
  earth: 1,
  mars: 1.8808158,
  jupiter: 11.862615,
  saturn: 29.447498,
  uranus: 84.016846,
  neptune: 164.79132,
};

const round = (num) => Math.round(100 * num) / 100;

export const age = (planet, ageInSeconds) => {
  if (! Object.hasOwn(planetaryYears, planet))
    throw new Error('not a planet');

  return round(ageInSeconds / secondsPerEarthYear / planetaryYears[planet]);
}
