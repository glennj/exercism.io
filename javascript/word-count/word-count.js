/* eslint-disable   class-methods-use-this */
/* eslint           no-param-reassign: ["error", { "props": false }] */

export default class Words {
  count(sentence) {
    const words = sentence.toLowerCase().trim().split(/\s+/u);
    const addMapping = (freq, word) => {
      freq[word] = (freq[word] || 0) + 1;
      return freq;
    };
    /* In order to have a completely "blank" object, we need to use
     * `Object.create(null)`. This new object has *no prototype*, therefore
     * we can add any property do it, even if it is a reserved word.
     * This would not be the case for the "empty object" literal: `{}`
     * which has properties like "constructor", etc.
     */
    return words.reduce(addMapping, Object.create(null));
  }
}
