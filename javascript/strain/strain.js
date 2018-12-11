/* eslint-disable  no-restricted-syntax, no-multi-spaces */

const keepOrDiscard = (list, func, wanted) => {
  const results = [];
  for (const elem of list) {
    if (func(elem) === wanted) {
      results.push(elem);
    }
  }
  return results;
};

const keep    = (list, func) => keepOrDiscard(list, func, true);
const discard = (list, func) => keepOrDiscard(list, func, false);

module.exports = { keep, discard };
