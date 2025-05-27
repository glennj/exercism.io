export const decodedValue = (values) =>
  values
    .slice(0, 2)
    .map(colorCode)
    .reduce((value, code) => value * 10 + code, 0);

/**
 * This is your code from the first resistor color exercise.
 */
const COLORS = [
  "black",
  "brown",
  "red",
  "orange",
  "yellow",
  "green",
  "blue",
  "violet",
  "grey",
  "white",
];

export const colorCode = (color) => COLORS.indexOf(color);
