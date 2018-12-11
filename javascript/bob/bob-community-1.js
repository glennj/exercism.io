const isSilent = m => m.length === 0;
const isShouting = m => m.toUpperCase() === m && m.match(/[A-Z\xc0-\xdf]/);
const isAsking = m => m.endsWith('?');
const isShoutAsking = m => isShouting(m) && isAsking(m);
const anything = () => true;

const CONDITIONS = [
  { question: isSilent, response: 'Fine. Be that way!' },
  { question: isShoutAsking, response: 'Calm down, I know what I\'m doing!' },
  { question: isShouting, response: 'Whoa, chill out!' },
  { question: isAsking, response: 'Sure.' },
  { question: anything, response: 'Whatever.' },
];

export const hey = (message) => {
  const { response } = CONDITIONS.find(c => c.question(message.trim()));
  return response;
};
