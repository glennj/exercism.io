const COLORS: string[] = [
  'black', 'brown', 'red', 'orange', 'yellow', 
  'green', 'blue', 'violet', 'grey', 'white'
];

export class ResistorColor {
  private colors: string[];

  constructor(colors: string[]) {
    if (colors.length < 2)
      throw new Error("At least two colors need to be present");
    this.colors = colors.slice(0, 2);
  }

  value = (): number => {
    let result = 0;
    this.colors.forEach((color: string) => {
      const code = COLORS.indexOf(color);
      if (code == -1)
        throw new Error("Unknown color: " + color);
      result = 10 * result + code;
    });
    return result; 
  };
}
