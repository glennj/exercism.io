import { from } from './iterable-range';

const CHAIN = [
  null,
  { name: 'fly' },
  { name: 'spider', tag: 'It wriggled and jiggled and tickled inside her.',
                    extra: ' that wriggled and jiggled and tickled inside her' },
  { name: 'bird',   tag: 'How absurd to swallow a bird!' },
  { name: 'cat',    tag: 'Imagine that, to swallow a cat!' },
  { name: 'dog',    tag: 'What a hog, to swallow a dog!' },
  { name: 'goat',   tag: 'Just opened her throat and swallowed a goat!' },
  { name: 'cow',    tag: 'I don\'t know how she swallowed a cow!' },
  { name: 'horse',  tag: 'She\'s dead, of course!',
                    stop: true },
];
Object.freeze(CHAIN);


function iKnow(animal) {
  return `I know an old lady who swallowed a ${animal.name}.\n`;
}
function hunt(predator, prey) {
  return `She swallowed the ${predator.name} to catch the ${prey.name}${prey.extra ?? ''}.\n`;
}
function cliffhanger() {
  return "I don't know why she swallowed the fly. Perhaps she'll die.\n";
}


export class Song {
  verse(n) {
    const animal = CHAIN[n];
    let v = iKnow(animal);
    if (animal.tag) v += `${animal.tag}\n`;
    if (!animal.stop) {
      from(n).downTo(2).forEach(i => {
        v += hunt(CHAIN[i], CHAIN[i - 1]);
      });
      v += cliffhanger();
    }
    return v;
  }

  verses(m, n) {
    return from(m).upTo(n).map(i => `${this.verse(i)}\n`).join('');
  }
}
