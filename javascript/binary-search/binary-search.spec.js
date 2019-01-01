import BinarySearch from './binary-search';

describe('BinarySearch', () => {
  const sortedArray = [1, 2, 3, 4, 5, 6];
  const sortedArrayOfOddLength = [0, 1, 2, 2, 3, 10, 12];
  const unsortedArray = [10, 2, 5, 1];

  it('should require a sorted array', () => {
    const invalidBinarySearch = new BinarySearch(unsortedArray);
    const validBinarySearch = new BinarySearch(sortedArray);

    expect(typeof invalidBinarySearch.array).toEqual('undefined');
    expect(Array.isArray(validBinarySearch.array)).toEqual(true);
    expect(invalidBinarySearch.indexOf(1)).toEqual(-1);
  });

  test('should find the correct index of an included value', () => {
    expect(new BinarySearch(sortedArray).indexOf(1)).toEqual(0);
    expect(new BinarySearch(sortedArray).indexOf(2)).toEqual(1);
    expect(new BinarySearch(sortedArray).indexOf(3)).toEqual(2);
    expect(new BinarySearch(sortedArray).indexOf(4)).toEqual(3);
    expect(new BinarySearch(sortedArray).indexOf(5)).toEqual(4);
    expect(new BinarySearch(sortedArray).indexOf(6)).toEqual(5);
  });

  test('should search the middle of the array', () => {
    expect(new BinarySearch(sortedArrayOfOddLength).indexOf(0)).toEqual(0);
    expect(new BinarySearch(sortedArrayOfOddLength).indexOf(1)).toEqual(1);
    expect([2, 3].includes(new BinarySearch(sortedArrayOfOddLength).indexOf(2))).toBeTruthy();
    expect(new BinarySearch(sortedArrayOfOddLength).indexOf(3)).toEqual(4);
    expect(new BinarySearch(sortedArrayOfOddLength).indexOf(10)).toEqual(5);
    expect(new BinarySearch(sortedArrayOfOddLength).indexOf(12)).toEqual(6);
  });

  test('should return -1 for a value not in the array', () => {
    expect(new BinarySearch(sortedArray).indexOf(0.5)).toEqual(-1);
    expect(new BinarySearch(sortedArray).indexOf(1.5)).toEqual(-1);
    expect(new BinarySearch(sortedArray).indexOf(2.5)).toEqual(-1);
    expect(new BinarySearch(sortedArray).indexOf(3.5)).toEqual(-1);
    expect(new BinarySearch(sortedArray).indexOf(4.5)).toEqual(-1);
    expect(new BinarySearch(sortedArray).indexOf(5.5)).toEqual(-1);
    expect(new BinarySearch(sortedArray).indexOf(6.5)).toEqual(-1);
  });
});
