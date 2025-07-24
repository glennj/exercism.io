// @ts-check
//
// The line above enables type checking for this file. Various IDEs interpret
// the @ts-check directive. It will give you helpful autocompletion when
// implementing this exercise.

import { ElectronicDevice } from './lib.js';

/**
 * Checks if input is a boolean.
 *
 * @param {unknown} value
 * @returns {boolean} whether the input is a boolean
 */
export const isBoolean = (value) => typeof(value) === 'boolean';

/**
 * Checks if input is a finite number or bigint.
 *
 * @param {unknown} value
 * @returns {boolean} whether the input is a finite number or bigint
 */
export const isNumber = (value) =>
  typeof(value) === 'bigint' ||
  (typeof(value) === 'number' && isFinite(value));

/**
 * Checks if a value is an object.
 *
 * @param {unknown} value
 * @returns {boolean} whether the input is an object.
 */
export const isObject = (value) => typeof(value) === 'object' && value !== null;

/**
 * Checks if a value is a numeric string.
 *
 * @param {unknown} value
 * @returns {boolean} whether the input is a numeric string.
 */
export function isNumericString(value) {
  if (typeof(value) !== 'string') return false;
  if (value.length === 0) return false;
  return !isNaN(Number(value));
}

/**
 * Checks if an object is an instance of the `ElectronicDevice` class or one of its children.
 *
 * @param {object} object
 * @returns {boolean} whether the object is an instance of the `ElectronicDevice` class or one of its children.
 */
export const isElectronic = (object) => object instanceof ElectronicDevice;

/**
 * Checks if a value is a non empty array.
 *
 * @param {unknown} value
 * @returns {boolean} whether the input is a non empty array.
 */
export const isNonEmptyArray = (value) => Array.isArray(value) && value.length > 0;

/**
 * Checks if a value is an empty array.
 *
 * @param {unknown} value
 * @returns {boolean} whether the input is an empty array.
 */
export const isEmptyArray = (value) => Array.isArray(value) && value.length === 0;

/**
 * Checks if a value has a "type" property or method.
 *
 * @param {object} object
 * @returns {boolean} whether the input has a "type" property or method.
 */
export const hasType = (object) => 'type' in object;

/**
 * Throws an error if an object is missing an "id" property or method.
 *
 * @param {object} object
 * @returns {never|void} undefined if the input has an "id" property or method, otherwise throws an error.
 */
export function assertHasId(object) {
  if ('id' in object) return;
  throw new Error();
}

/**
 * Checks if a value has an "id" property.
 *
 * @param {object} object
 * @returns {boolean} whether the input has an "id" property.
 */
export const hasIdProperty = (object) => Object.hasOwn(object, 'id');

/**
 * Checks if a value has a defined "type" property.
 *
 * @param {object} object
 * @returns {boolean} whether the input has a defined "type" property.
 */
export const hasDefinedType = (object) =>
  Object.hasOwn(object, 'type') && typeof(object.type) !== 'undefined';
