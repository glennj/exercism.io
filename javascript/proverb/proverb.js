import { ExtendedArray } from './extended-array';

export function proverb(...words) {
  let qualifier = '';
  if (typeof words[words.length - 1] === 'object')
    qualifier = `${words.pop().qualifier} `;

  const lines = [];
  for (const [first, second] of ExtendedArray.from(words).eachConsecutive(2)) 
    lines.push(`For want of a ${first} the ${second} was lost.`);

  if (words.length > 0)
    lines.push(`And all for the want of a ${qualifier}${words[0]}.`);

  return lines.join('\n');
}
