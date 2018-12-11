/* eslint-disable  object-curly-newline, object-property-newline, indent, no-multi-spaces  */

// ref: https://exercism.io/tracks/javascript/exercises/beer-song/solutions/146b391174db4b49a1e559db9096273
import { from } from '../lib/my-range-iterable';

// ref: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
function template(strings, ...keys) {
  return ((...values) => {
    const dict = values[values.length - 1] || {};
    const result = [strings[0]];
    keys.forEach((key, i) => {
      const value = Number.isInteger(key) ? values[key] : dict[key];
      result.push(value, strings[i + 1]);
    });
    return result.join('');
  });
}

class Song {
  constructor() {
    const chain = [null];
    chain.push({ name: 'fly' });
    chain.push({ name: 'spider', tag: 'It wriggled and jiggled and tickled inside her.',
                                 extra: ' that wriggled and jiggled and tickled inside her' });
    chain.push({ name: 'bird',   tag: 'How absurd to swallow a bird!' });
    chain.push({ name: 'cat',    tag: 'Imagine that, to swallow a cat!' });
    chain.push({ name: 'dog',    tag: 'What a hog, to swallow a dog!' });
    chain.push({ name: 'goat',   tag: 'Just opened her throat and swallowed a goat!' });
    chain.push({ name: 'cow',    tag: 'I don\'t know how she swallowed a cow!' });
    chain.push({ name: 'horse',  tag: 'She\'s dead, of course!',
                                 stop: true });
    this.chain = chain;

    // tagged templates
    this.iKnow       = template`I know an old lady who swallowed a ${0}.\n`;
    this.newline     = template`${0}\n`;
    this.hunt        = template`She swallowed the ${0} to catch the ${1}${2}.\n`;
    this.cliffhanger = 'I don\'t know why she swallowed the fly. Perhaps she\'ll die.\n';
  }

  verse(n) {
    const animal = this.chain[n];
    let v = this.iKnow(animal.name);
    if (animal.tag) v += this.newline(animal.tag);
    if (!animal.stop) {
      from(n).downTo(2).forEach((i) => {
        const predator = this.chain[i];
        const prey = this.chain[i - 1];
        v += this.hunt(predator.name, prey.name, (prey.extra || ''));
      });
      v += this.cliffhanger;
    }
    return v;
  }

  verses(m, n) {
    return from(m).upTo(n).map(i => this.newline(this.verse(i))).join('');
  }
}

module.exports = Song;
