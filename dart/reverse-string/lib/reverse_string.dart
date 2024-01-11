String reverse(String input) =>
    input.split('').fold('', (rev, char) => '$char$rev');
