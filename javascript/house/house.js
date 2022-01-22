import {from} from './iterable-range';

const actors = [
  { who: 'house that Jack built.' },
  { who: 'malt', what: 'lay in' },
  { who: 'rat', what: 'ate' },
  { who: 'cat', what: 'killed' },
  { who: 'dog', what: 'worried' },
  { who: 'cow with the crumpled horn', what: 'tossed' },
  { who: 'maiden all forlorn', what: 'milked' },
  { who: 'man all tattered and torn', what: 'kissed' },
  { who: 'priest all shaven and shorn', what: 'married' },
  { who: 'rooster that crowed in the morn', what: 'woke' },
  { who: 'farmer sowing his corn', what: 'kept' },
  { who: 'horse and the hound and the horn', what: 'belonged to' },
];

const verse = (n) => {
  const lines = [`This is the ${actors[n - 1].who}`];
  for (let i = n - 1; i >= 1; i -= 1) {
    lines.push(`that ${actors[i].what} the ${actors[i - 1].who}`);
  }
  return lines;
};

const verses = (m = 1, n = actors.length) => {
  const lyrics = from(m)
                 .upTo(n)
                 .reduce((lyrics, i) => lyrics.concat(verse(i), ''), []);
  lyrics.pop();
  return lyrics;
};

module.exports = {House: {verse, verses}};
