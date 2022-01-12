/* A translation of my Tcl solution, which is a translation of the Java reference solution.
 * Sorry not sorry.
 *    https://exercism.org/tracks/tcl/exercises/book-store/solutions/glennj
 *    https://github.com/exercism/java/blob/master/exercises/book-store/.meta/src/reference/java/BookStore.java
 */

import {ExtendedArray} from './ExtendedArray';

const BOOK_COST = 800;
const DISCOUNTS = {1: 0, 2: 5, 3: 10, 4: 20, 5: 25};

// the cost of n copies of a book, applying the discount
const GROUP_COST = {};
for (const [n, d] of Object.entries(DISCOUNTS)) {
  GROUP_COST[n] = n * BOOK_COST * (100 - d)/100;
}

// the recursive heart of the problem
const calculateCost = function(books, costSoFar) {
  if (books.isEmpty()) {
    return costSoFar;
  }

  const uniqBooks = books.distinct();
  let minCost = Infinity;

  for (let i = 1; i <= uniqBooks.length; i++) {
    const newGroupBooks = uniqBooks.slice(0, i);
    const remainingBooks = books.slice();
    newGroupBooks.forEach(book => remainingBooks.removeFirst(book));
    const cost = calculateCost(remainingBooks, costSoFar + GROUP_COST[i]);
    minCost = Math.min(cost, minCost);
  }

  return minCost;
};

export const cost = (books) => {
  let ordered = new ExtendedArray(...books).orderedByCount();
  return calculateCost(ordered, 0);
};
