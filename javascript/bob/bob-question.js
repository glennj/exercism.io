/* eslint-disable no-unused-vars */

export const hey = (message) => {
  const isEmpty = message.trim().length === 0;
  const upper = message.toUpperCase();
  const isUpper = message === upper && /[A-Z]/.test(upper);
  const isQuestion = message.trim().endsWith('?');

  if (isEmpty) {
    return 'Fine. Be that way!';
  }
  if (isUpper && isQuestion) {
    return 'Calm down, I know what I\'m doing!';
  }
  if (isUpper) {
    return 'Whoa, chill out!';
  }
  if (isQuestion) {
    return 'Sure.';
  }
  return 'Whatever.';
};
