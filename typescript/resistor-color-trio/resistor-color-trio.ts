// Reuse previous solutions.
import {colorCode, ResistorColor} from './previous-solutions'

const valueOf = ([first, second, third]: string[]): number => {
  const resistorValue = new ResistorColor([first, second]).value()
  const exponent = colorCode(third)
  return resistorValue * 10 ** exponent
}

const prefixes = ['', 'kilo', 'mega', 'giga']

const withUnits = (value: number, unit: string): string => {
  let idx = 0

  while (value % 1000 == 0) {
    value /= 1000
    idx++
  }

  return `${value} ${prefixes[idx]}${unit}`
}

export const decodedResistorValue = (colors: string[]): string =>
  withUnits(valueOf(colors), 'ohms')
