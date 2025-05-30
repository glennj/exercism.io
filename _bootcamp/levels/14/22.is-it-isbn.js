export function isValidIsbn(isbn) {
  let idx = 0;
  let sum = 0;

  for (const char of isbn) {
    if (char === "-") continue;
    idx++;
    let digit = Number.parseInt(char, 10);
    if (Number.isNaN(digit)) {
      if (idx === 10 && char === "X") digit = 10;
      else return false;
    }
    sum += idx * digit;
  }

  return idx === 10 && sum % 11 === 0;
}
