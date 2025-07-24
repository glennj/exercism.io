// @ts-check

/**
 * Create an appointment
 *
 * @param {number} days
 * @param {number} [now] (ms since the epoch, or undefined)
 *
 * @returns {Date} the appointment
 */
export function createAppointment(days, now = Date.now()) {
  const date = new Date(now);
  // Happily, javascript dates compensate for dates greater than
  // the number of days in a month.
  // Use UTC dates so that we're not bothered by daylight saving time.
  date.setUTCDate(date.getUTCDate() + days);
  return date;
}

/**
 * Generate the appointment timestamp
 *
 * @param {Date} appointmentDate
 *
 * @returns {string} timestamp
 */
export function getAppointmentTimestamp(appointmentDate) {
  return appointmentDate.toISOString();
}

/**
 * Get details of an appointment
 *
 * @param {string} timestamp (ISO 8601)
 *
 * @returns {Record<'year' | 'month' | 'date' | 'hour' | 'minute', number>} the appointment details
 */
export function getAppointmentDetails(timestamp) {
  const date = new Date(timestamp);
  return {
    year: date.getFullYear(),
    month: date.getMonth(),
    date: date.getDate(),
    hour: date.getHours(),
    minute: date.getMinutes(),
  };
}

/**
 * Update an appointment with given options
 *
 * @param {string} timestamp (ISO 8601)
 * @param {Partial<Record<'year' | 'month' | 'date' | 'hour' | 'minute', number>>} options
 *
 * @returns {Record<'year' | 'month' | 'date' | 'hour' | 'minute', number>} the appointment details
 */
export function updateAppointment(timestamp, options) {
  const date = new Date(timestamp);

  for (const param in options) {
    switch (param) {
      case 'year':   date.setFullYear(options.year);  break;
      case 'month':  date.setMonth(options.month);    break;
      case 'date':   date.setDate(options.date);      break;
      case 'hour':   date.setHours(options.hour);     break;
      case 'minute': date.setMinutes(options.minute); break;
    }
  }

  return getAppointmentDetails(date.toISOString());
}

/**
 * Get available time in seconds (rounded) between two appointments
 *
 * @param {string} timestampA (ISO 8601)
 * @param {string} timestampB (ISO 8601)
 *
 * @returns {number} amount of seconds (rounded)
 */
export function timeBetween(timestampA, timestampB) {
  const diffMs = new Date(timestampA).getTime() - new Date(timestampB).getTime();
  return Math.abs(Math.round(diffMs / 1000));
}

/**
 * Determine if the appointment is in the future, compared to the current time.
 *
 * @param {string} appointmentTimestamp (ISO 8601)
 * @param {string} currentTimestamp (ISO 8601)
 *
 * @returns {boolean} appointment is in the future
 */
export function isValid(appointmentTimestamp, currentTimestamp) {
  return new Date(appointmentTimestamp).getTime() > new Date(currentTimestamp).getTime();
}
