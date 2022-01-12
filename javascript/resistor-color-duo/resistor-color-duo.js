// reuse the "resistor color" exercise
import { colorCode } from './resistor-color.js';

export const decodedValue = (colors) => {
  return colors
    .slice(0,2)
    .map(color => colorCode(color))
    .reduce((val, code) => val * 10 + code, 0);
};
