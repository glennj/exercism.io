let value = colors =>
  colors
  ->Array.slice(~end=2)
  ->Array.map(ResistorColor.colorCode)
  ->Array.reduce(0, (sum, code) => sum * 10 + code)
