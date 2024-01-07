String reverse(String input) {
  return input.split('')
              .fold([], (rev, char) { rev.insert(0, char); return rev; })
              .join('');
}
