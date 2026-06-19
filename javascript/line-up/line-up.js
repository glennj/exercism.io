const ordinal = (n) => {
  const [ones, tens] = [n % 10, n % 100];
  let suffix = "th";
  if (ones === 1 && tens !== 11) suffix = "st";
  if (ones === 2 && tens !== 12) suffix = "nd";
  if (ones === 3 && tens !== 13) suffix = "rd";
  return `${n}${suffix}`;
}

export const format = (name, number) =>
  `${name}, you are the ${ordinal(number)} customer we serve today. Thank you!`;
