const billionSeconds = 10 ** 9;
export const gigasecond = date => new Date(date.valueOf() + billionSeconds * 1000);
