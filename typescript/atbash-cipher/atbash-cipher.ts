const AZ = '0123456879_abcdefghijklmnopqrstuvwxyz'
const ZA = '0123456879_zyxwvutsrqponmlkjihgfedcba'

export function encode(str: string): string {
  return groupBy5( code(str) )
}

export function decode(str: string): string {
  return code(str)
}

function code(str: string): string {
  return str
    .toLocaleLowerCase()
    .replace(/\W/g, '')
    .split('')
    .map(codeChar)
    .join('')
}

function codeChar(c: string): string {
  return ZA[AZ.indexOf(c)]
}

function groupBy5(str: string): string {
  return (str.match(/.{1,5}/g) || []).join(' ')
}
