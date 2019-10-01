// reuse the "resistor color" exercise
import { colorCode } from '../resistor-color/resistor-color.js';

export class ResistorColorTrio {
  constructor(colors) {
    let x, y, z;
    try {
      [x, y, z] = colors.map(c => colorCode(c));
    } catch (e) {
      throw new Error("invalid color");
    }
    this.value = (10 * x + y) * (10 ** z);
  }

  get label() {
    let val = this.value;
    let magnitude = 0;
    while (val > 0 && val % 1000 == 0) {
      val /= 1000;
      magnitude++;
    }
    const prefix = ["", "kilo", "mega", "giga"][magnitude];

    return `Resistor value: ${val} ${prefix}ohms`;
  }
}
