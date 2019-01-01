/* eslint-disable  no-extend-native, func-names, no-restricted-syntax */

// inspired by https://ruby-doc.org/core-2.5.3/Enumerable.html#method-i-each_cons
Array.prototype.eachConsecutive = function* (n) {
  for (let i = 0; i <= this.length - n; i += 1) {
    yield this.slice(i, i + n);
  }
};

function proverb(...words) {
  let qualifier = '';
  if (typeof words[words.length - 1] === 'object') {
    qualifier = `${words.pop().qualifier} `;
  }

  return [...words.eachConsecutive(2)]
    .map(([w1, w2]) => `For want of a ${w1} the ${w2} was lost.`)
    .concat([`And all for the want of a ${qualifier}${words[0]}.`])
    .join('\n');
}

module.exports = proverb;
