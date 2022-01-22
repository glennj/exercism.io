/* an acronym generator: 'Ruby on Rails' => 'ROR'
 */
const parse = phrase => phrase
  .replace(/\b[a-z]/g, c => c.toUpperCase())
  .replace(/\b([A-Z])([A-Z]+)\b/g, (_, c, rest) => c + rest.toLowerCase())
  .replace(/[^A-Z]/g, '');

module.exports = { parse };
