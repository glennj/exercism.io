/* - `replaceAll` available in node 15.0.0
 * - exercism javascript test runner is on node v12.x
 *   as of 2022-01-28
 */

const hasReplaceAll = String.prototype.replaceAll instanceof Function;

const trimQuotes = (word) => {
  return hasReplaceAll
          ? word.replaceAll(/^'|'$/g, '')
          : word.replace(/^'/, '').replace(/'$/, '');
}

export const countWords = (sentence) => {
  return Array.from(sentence.matchAll(/[a-z0-9']+/ig))
    .map(matchResult => matchResult[0])
    .map(trimQuotes)
    .map(word => word.toLowerCase())
    .reduce((count, word) => {
      count[word] = (count[word] ?? 0) + 1;
      return count
    }, {});
};
