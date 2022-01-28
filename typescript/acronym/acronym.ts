export const parse = (phrase: string): string => {
  let acronym = ''
  phrase.split(/[ -]/).forEach((word) => {
    acronym += word[0]
    if (word !== word.toUpperCase()) {
      acronym += word.substr(1).replace(/[^A-Z]/g, '')
    }
  })
  return acronym.toUpperCase()
};
