
function translate (phrase: string): string {
  return phrase
    .split(/\s+/)
    .map((word) => translateWord(word.toLowerCase()))
    .join(' ')
}

function translateWord(word: string): string {
  // "apple" => "appleay", "xray" => "xrayay"
  let m = word.match(/^(?:[aeiou]|xr|yt)/)
  if (m) { return `${word}ay` }

  // 1. "square" => "aresquay", "quip" => "ipquay"
  // 2. "rhythm" => "ythmrhay", "my" => "ymay"
  // 3. "strengths" => "engthsstray"
  m = word.match(/^(.?qu)(.*)/)
   || word.match(/^([^aeiou]+)(y.*)/)
   || word.match(/^([^aeiou]+)(.*)/)
  if (m) { return `${m[2]}${m[1]}ay` }

  // are there any other cases?
  return `${word}ay`
}

export default { translate }
