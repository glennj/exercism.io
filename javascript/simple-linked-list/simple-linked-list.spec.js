import { beforeEach, describe, expect, test, test } from '@jest/globals';
import { Element, List } from './simple-linked-list';

describe('Element class', () => {
  test('has constructor', () => {
    const element = new Element(1);
    expect(element.value).toEqual(1);
  });

  test('value reflects constructor arg', () => {
    const element = new Element(2);
    expect(element.value).toEqual(2);
  });

  test('has null for next by default', () => {
    const element = new Element(1);
    expect(element.next).toEqual(null);
  });
});

describe('List class', () => {
  test('has constructor', () => {
    const list = new List();
    expect(list).toBeDefined();
  });

  test('new lists should have length 0', () => {
    const list = new List();
    expect(list.length).toEqual(0);
  });

  test('can add a element', () => {
    const list = new List();
    const element = new Element(1);
    expect(() => list.add(element)).not.toThrow();
  });

  test('adding a element increments length', () => {
    const list = new List();
    const element = new Element(1);
    list.add(element);
    expect(list.length).toEqual(1);
  });

  test('adding two elements increments twice', () => {
    const list = new List();
    const element1 = new Element(1);
    const element2 = new Element(3);
    list.add(element1);
    list.add(element2);
    expect(list.length).toEqual(2);
  });

  test('new Lists have a null head element', () => {
    const list = new List();
    expect(list.head).toEqual(null);
  });

  test('adding an Element to an empty list sets the head Element', () => {
    const list = new List();
    const element = new Element(1);
    list.add(element);
    expect(list.head.value).toEqual(1);
  });

  test('adding a second Element updates the head Element', () => {
    const list = new List();
    const element1 = new Element(1);
    const element2 = new Element(3);
    list.add(element1);
    list.add(element2);
    expect(list.head.value).toEqual(3);
  });

  test('can get the next Element from the head', () => {
    const list = new List();
    const element1 = new Element(1);
    const element2 = new Element(3);
    list.add(element1);
    list.add(element2);
    expect(list.head.next.value).toEqual(1);
  });

  test('can be initialized with an array', () => {
    const list = new List([1, 2, 3]);
    expect(list.length).toEqual(3);
    expect(list.head.value).toEqual(3);
  });
});

describe('Lists with multiple elements', () => {
  let list;
  beforeEach(() => {
    list = new List([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
  });
  test('with correct length', () => {
    expect(list.length).toEqual(10);
  });
  test('with correct head value', () => {
    expect(list.head.value).toEqual(10);
  });
  test('can traverse the list', () => {
    expect(list.head.next.next.next.value).toEqual(7);
  });
  test('can convert to an array', () => {
    const oneList = new List([1]);
    expect(oneList.toArray()).toEqual([1]);
  });
  test('head of list is final element from input array', () => {
    const twoList = new List([1, 2]);
    expect(twoList.head.value).toEqual(2);
  });
  test('can convert longer list to an array', () => {
    expect(list.toArray()).toEqual([10, 9, 8, 7, 6, 5, 4, 3, 2, 1]);
  });
  test('can be reversed', () => {
    const twoList = new List([1, 2]);
    expect(twoList.reverse().toArray()).toEqual([1, 2]);
  });
  test('can be reversed when it has more elements', () => {
    const threeList = new List([1, 2, 3]);
    expect(threeList.reverse().toArray()).toEqual([1, 2, 3]);
  });
  test('can reverse with many elements', () => {
    expect(list.reverse().toArray()).toEqual([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
  });
  test('can reverse a reversal', () => {
    expect(list.reverse().reverse().toArray()).toEqual([
      10, 9, 8, 7, 6, 5, 4, 3, 2, 1,
    ]);
  });
});
