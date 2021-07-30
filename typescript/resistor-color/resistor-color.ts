export const COLORS: string[] = [
  'black', 'brown', 'red', 'orange', 'yellow', 
  'green', 'blue', 'violet', 'grey', 'white'
];

export const colorCode = (color: string): number => {
  const code = COLORS.indexOf(color);
  if (code == -1)
    throw new Error("Invalid color: " + color);
  return code;
}
