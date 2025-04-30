const HOURS_PER_DAY = 8;
const BILLABLE_DAYS_PER_MONTH = 22;

export function dayRate(ratePerHour) {
  return HOURS_PER_DAY * ratePerHour;
}

export function daysInBudget(budget, ratePerHour) {
  return Math.floor(budget / dayRate(ratePerHour));
}

export function priceWithMonthlyDiscount(ratePerHour, numDays, discount) {
  let dayRate = ratePerHour * HOURS_PER_DAY;
  let daysInWholeMonths = numDays - numDays % BILLABLE_DAYS_PER_MONTH;

  let discountedAmount = daysInWholeMonths * dayRate * (1 - discount);
  let fullPriceAmount = (numDays - daysInWholeMonths) * dayRate;
  
  return Math.ceil(discountedAmount + fullPriceAmount);
}
