/* eslint-disable  no-restricted-syntax */

const accumulate = (list, func) => {
  const result = [];
  for (const element of list) { result.push(func(element)); }
  return result;
};

module.exports = { accumulate };
