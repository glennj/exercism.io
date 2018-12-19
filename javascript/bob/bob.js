/* eslint-disable no-unused-vars */

export const hey = (message) => {
  const isEmpty = message.trim().length === 0;
  const isYelling = message === message.toUpperCase() && /[A-Z]/.test(message);
  const isQuestion = message.trim().endsWith('?');

  if (isEmpty) {
    return 'Fine. Be that way!';
  }
  if (isYelling && isQuestion) {
    return 'Calm down, I know what I\'m doing!';
  }
  if (isYelling) {
    return 'Whoa, chill out!';
  }
  if (isQuestion) {
    return 'Sure.';
  }
  return 'Whatever.';
};
