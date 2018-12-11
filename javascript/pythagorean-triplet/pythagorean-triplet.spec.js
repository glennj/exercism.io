import Triplet from './pythagorean-triplet';

describe('Triplet', () => {
  test('calculates the sum', () => {
    expect(new Triplet(3, 4, 5).sum()).toBe(12);
  });

  test('calculates the product', () => {
    expect(new Triplet(3, 4, 5).product()).toBe(60);
  });

  test('can recognize a pythagorean triplet', () => {
    expect(new Triplet(3, 4, 5).isPythagorean()).toBe(true);
  });

  test('can recognize a non pythagorean triplet', () => {
    expect(new Triplet(5, 6, 7).isPythagorean()).toBe(false);
  });

  test('can make triplets up to 10', () => {
    const triplets = Triplet.where({ maxFactor: 10 });
    const products = triplets.map(triplet => triplet.product()).sort((a, b) => a - b);
    expect(products).toEqual([60, 480]);
  });

  test('can make triplets 11 through 20', () => {
    const triplets = Triplet.where({ minFactor: 11, maxFactor: 20 });
    const products = triplets.map(triplet => triplet.product()).sort((a, b) => a - b);
    expect(products).toEqual([3840]);
  });

  test('can filter on sum', () => {
    const triplets = Triplet.where({ sum: 180, maxFactor: 100 });
    const products = triplets.map(triplet => triplet.product()).sort((a, b) => a - b);
    expect(products).toEqual([118080, 168480, 202500]);
  });

  test('Pythagorean triangle where sum == 1000', () => {
    const triplets = Triplet.where({ sum: 1000, maxFactor: 500 });
    expect(triplets.length).toEqual(1);
    const t = triplets[0];
    expect(t.sum()).toEqual(1000);
    expect(t.product()).toEqual(31875000);
    const s = t.toString();
    expect(/\b200\b/.test(s) && /\b375\b/.test(s) && /\b425\b/.test(s)).toBe(true);
  });
});
