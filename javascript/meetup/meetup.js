/* eslint-disable  object-curly-newline, quote-props */
/* eslint-disable  no-param-reassign */

/* the 1st monday of the month is the monday on or after day 1 of this month
 * the 2st monday of the month is the monday on or after day 8 of this month
 * ...
 * the 'teenth'  monday of the month is the monday on or after day 13 of this month
 */
const startDay = { 
  first:   1,
  second:  8,
  third:  15,
  fourth: 22,
  fifth:  29,
  teenth: 13
};

// get the weekday names without hardcoding.
// TODO How to query the current locale?
const locale = 'en-US';
const weekdays = new Array(7);
for (let i = 1; i <= 7; i += 1) {
  const date = new Date(1970, 0, i);
  weekdays[date.getDay()] = date.toLocaleDateString(locale, { weekday: 'long' }).toLowerCase();
}

/* javascript does relative days conveniently:
 * - the 0th day of this month renders as the _last_ day of _last_ month
 * - the month number 12 of this year is Jan of _next_ year
 */
const lastMonthDay = (year, month) => new Date(year, month + 1, 0).getDate();

const dayOnOrAfter = (year, month, dayOfWeek, startday) => {
  let date = new Date(year, month, startday);
  const delta = (dayOfWeek - date.getDay() + 7) % 7;
  if (delta > 0) {
    date = new Date(year, month, startday + delta);
    if (date.getMonth() !== month) throw new Error('no such day this month');
  }
  return date;
};

export const meetup = (year, monthNum, nth, weekday) => {
  const dayOfWeek = weekdays.indexOf(weekday.toLowerCase());
  if (dayOfWeek === -1) throw new Error('invalid weekday');

  // Month number out-of-range will not cause an error for creating dates.
  // However, a "wrong" month will mess up the logic of dayOnOrAfter.
  if (monthNum < 1 || monthNum > 12) throw new Error('month out of range (1-12)');

  // we're passed the monthNum where 1 is Jan and 12 is Dec.
  // javascript uses 0 => Jan and 11 => Dec
  monthNum -= 1;

  if (nth === 'last') {
    return dayOnOrAfter(year, monthNum, dayOfWeek, lastMonthDay(year, monthNum) - 6);
  }
  if (startDay[nth]) {
    return dayOnOrAfter(year, monthNum, dayOfWeek, startDay[nth]);
  }
  throw new Error('invalid descriptor');
};
