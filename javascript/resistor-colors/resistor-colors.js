// reuse the "resistor color" exercise
import { colorCode } from '../resistor-color/resistor-color.js';

export const value = (colors) => {
  return colors.reduce((val, color) => {
    return val * 10 + colorCode(color);
  }, 0);
};
