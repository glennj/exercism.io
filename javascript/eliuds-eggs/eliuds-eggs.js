export const eggCount = (displayValue) => {
  /* iteratively: */
  let count = 0;
  while (displayValue > 0) {
    count += displayValue & 1;
    displayValue >>= 1;
  }
  return count;

  /* functionally 
  return displayValue
    .toString(2)
    .split('')
    .filter((digit) => digit == "1")
    .length;
  */
};
