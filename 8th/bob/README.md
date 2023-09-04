# Bob

Welcome to Bob on Exercism's 8th Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Introduction

Bob is a [lackadaisical][] teenager.
He likes to think that he's very cool.
And he definitely doesn't get excited about things.
That wouldn't be cool.

When people talk to him, his responses are pretty limited.

[lackadaisical]: https://www.collinsdictionary.com/dictionary/english/lackadaisical

## Instructions

Your task is to determine what Bob will reply to someone when they say something to him or ask him a question.

Bob only ever answers one of five things:

- **"Sure."**
  This is his response if you ask him a question, such as "How are you?"
  The convention used for questions is that it ends with a question mark.
- **"Whoa, chill out!"**
  This is his answer if you YELL AT HIM.
  The convention used for yelling is ALL CAPITAL LETTERS.
- **"Calm down, I know what I'm doing!"**
  This is what he says if you yell a question at him.
- **"Fine. Be that way!"**
  This is how he responds to silence.
  The convention used for silence is nothing, or various combinations of whitespace characters.
- **"Whatever."**
  This is what he answers to anything else.

## Running and testing your solutions
 
### From the command line
 
Simply type `8th test.8th`. It is assumed that the 8th binary is declared in the PATH environment variable.
 
### From a REPL
 
Start 8th loading test-words.8th: `8th -f test-words.8th`.
This will start a CLI session where, once you’ve written some code in your solution file, you can load it with `"bob.8th" f:include`
and you can run the tests with `"bob-tests.8th" f:include` or you can copy and paste a single test from that file or enter your own. 
 
### Editing the bob-tests.8th file
 
This is encouraged at the beginning of your journey. Insert a back-slash space before all but the first one or two tests. Write your code to solve the first test or two. Then progressively enable more tests until you can pass all of them.

## Source

### Created by

- @axtens

### Based on

Inspired by the 'Deaf Grandma' exercise in Chris Pine's Learn to Program tutorial. - https://pine.fm/LearnToProgram/?Chapter=06