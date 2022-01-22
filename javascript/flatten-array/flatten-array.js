const flattener = (result, elem) => {
  if (elem !== null && elem !== undefined) {
    result = result.concat(
      Array.isArray(elem)
        ? flatten(elem)
        : elem
    );
  }
  return result;
};

export const flatten = (list) => list.reduce(flattener, []);
