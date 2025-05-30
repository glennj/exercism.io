const isEven = (n) => n % 2 === 0;

export function collatzSteps(number, steps = 0) {
  if (number < 1) throw new Exception('number cannot be less than 1');
  if (number === 1) return steps;
  if (isEven(number)) return collatzSteps(number / 2, steps + 1);
  return collatzSteps(3 * number + 1, steps + 1);
}
