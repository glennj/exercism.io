export const countWords = (sentence) => {
  return Array.from(sentence.matchAll(/[a-z0-9']+/ig))
    .map(matchResult => matchResult[0])
    .map(word => word.replaceAll(/^'|'$/g, ''))
    .filter(word => word.length > 0)
    .map(word => word.toLowerCase())
    .reduce((count, word) => {
      count[word] = (count[word] ?? 0) + 1;
      return count
    }, {});
};
