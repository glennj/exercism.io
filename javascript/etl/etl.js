/* eslint no-param-reassign: ["error", { "props": false }] */

export const transform = (old) => {

  /*
  //straightfowardly

  const result = {};
  for (const [points, letters] of Object.entries(old)) {
    const value = parseInt(points, 10);
    letters.forEach(letter => {
      result[letter.toLowerCase()] = value;
    });
  }
  return result;
  */

  // functionally

  const transformEntries = (newEntries, [points, letters]) => {
    const value = Number.parseInt(points, 10);
    return newEntries.concat(letters.map(l => {
      return [l.toLowerCase(), value];
    }));
  };

  const entriesToObject = (obj, [key, value]) => {
    obj[key] = value;
    return obj;
  };

  return Object.entries(old)
    .reduce(transformEntries, [])
    .reduce(entriesToObject, {});
};
