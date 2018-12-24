export default function (str) {
  let reversed = '';
  for (let i = str.length - 1; i >= 0; i -= 1) {
    reversed = reversed.concat(str[i]);
  }
  return reversed;
}
