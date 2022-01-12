const COLORS: string[] = [
  'black', 'brown', 'red', 'orange', 'yellow', 
  'green', 'blue', 'violet', 'grey', 'white'
];

export const colorCode = (color: string): number => {
  const code = COLORS.indexOf(color);

  if (code == -1)
    throw new Error("Invalid color: " + color);

  return code;
}

export class ResistorColor {
  private colors: string[];

  constructor(colors: string[]) {
    if (colors.length < 2)
      throw new Error("At least two colors need to be present");

    this.colors = colors.slice(0, 2);
  }

  value = (): number => {
    return this.colors.reduce(
      (sum: number, color: string) => 10 * sum + colorCode(color),
      0
    );
  };
}
