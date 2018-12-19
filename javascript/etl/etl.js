/* eslint no-param-reassign: ["error", { "props": false }] */

export default function transform(old) {
  const transformEntries = (newEntries, [points, letters]) => {
    const pointValue = Number.parseInt(points, 10);
    return newEntries.concat(letters.map(l => [l.toLowerCase(), pointValue]));
  };

  const entriesToObject = (obj, [key, value]) => {
    obj[key] = value;
    return obj;
  };

  return Object.entries(old)
    .reduce(transformEntries, [])
    .reduce(entriesToObject, {});
}
