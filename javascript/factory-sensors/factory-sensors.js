// @ts-check

export class ArgumentError extends Error {}

export class OverheatingError extends Error {
  constructor(temperature) {
    super(`The temperature is ${temperature} ! Overheating !`);
    this.temperature = temperature;
  }
}

const HUMIDITY_THRESHOLD = 70;      // percent
const TEMPERATURE_THRESHOLD = 500;  // degrees Celsius
const TEMPERATURE_CRITICAL = 600;  // degrees Celsius

/**
 * Check if the humidity level is not too high.
 *
 * @param {number} humidityPercentage
 * @throws {Error}
 */
export function checkHumidityLevel(humidityPercentage) {
  if (humidityPercentage > HUMIDITY_THRESHOLD)
    throw new Error('Humidity too high');
}

/**
 * Check if the temperature is not too high.
 *
 * @param {number|null} temperature
 * @throws {ArgumentError|OverheatingError}
 */
export function reportOverheating(temperature) {
  if (temperature === null)
    throw new ArgumentError('temperature is null');
  if (temperature > TEMPERATURE_THRESHOLD)
    throw new OverheatingError(temperature);
}

/**
 *  Triggers the needed action depending on the result of the machine check.
 *
 * @param {{
 * check: function,
 * alertDeadSensor: function,
 * alertOverheating: function,
 * shutdown: function
 * }} actions
 * @throws {ArgumentError|OverheatingError|Error}
 */
export function monitorTheMachine(actions) {
  try {
    actions.check();
  } catch (e) {
    if (e instanceof OverheatingError) {
      if (e.temperature > TEMPERATURE_CRITICAL)
        actions.shutdown();
      else
        actions.alertOverheating();
    }
    else if (e instanceof ArgumentError) {
      actions.alertDeadSensor();
    }
    else {
      throw e;
    }
  }
}
