#!/usr/bin/env node

const fs = require('fs');

const opts = {
  lineNumbers:     false, // -n
  caseInsensitive: false, // -i
  fileNameOnly:    false, // -l
  wholeLine:       false, // -x
  inverted:        false, // -v
};

function main() {
  let args = parseOptions(process.argv.slice(2));
  if (args.length < 2) {
    console.error('usage: grep.js [options] pattern file [...]');
    process.exit(1);
  }
  let pattern = getPattern(args.shift());
  args.forEach(filename => {
    processFile(filename, pattern, args.length);
  });
}

function parseOptions(args) {
  while (args[0].startsWith('-')) {
    let option = args.shift();
    switch (option) {
      case '-n':
        opts.lineNumbers = true;
        break;
      case '-i':
        opts.caseInsensitive = true;
        break;
      case '-l':
        opts.fileNameOnly = true;
        break;
      case '-x':
        opts.wholeLine = true;
        break;
      case '-v':
        opts.inverted = true;
        break;
      default:
        console.error('Error: unknown option ' + option);
        process.exit(1);
    }
  }
  return args;
}

function getPattern(patternString) {
  if (opts.wholeLine) {
    patternString = '^' + patternString + '$';
  }
  return new RegExp(patternString, opts.caseInsensitive ? 'i' : '');
}

function processFile(filename, pattern, numFiles) {
  // TODO: validate file exists and is readable
  const lines = fs.readFileSync(filename, 'utf8').split('\n');

  // XOR is a bitwise operator that requires ints, not booleans.
  const inv = opts.inverted ? 1 : 0;

  let lineNum = 1;
  for (const line of lines) {
    const match = line.match(pattern) ? 1 : 0;
    if (inv ^ match) {
      emit(filename, line, lineNum, numFiles);
      if (opts.fileNameOnly) {
        break;
      }
    }
    lineNum += 1;
  }
}

function emit(filename, line, lineNum, numFiles) {
  const output = [];
  if (opts.fileNameOnly) {
    output.push(filename);
  }
  else {
    if (numFiles > 1) {
      output.push(filename);
    }
    if (opts.lineNumbers) {
      output.push(lineNum);
    }
    output.push(line);
  }
  console.log(output.join(':'));
}

main();
