# Hints

## General

The SGF language has a recursive definition:

```none
GameTree = "(" Sequence { GameTree } ")"
```

That is, a GameTree is a piece of text that starts with a `(`, followed by some text called a "Sequence" (a list of game nodes), _followed by **zero or more** child GameTrees_, and ends with a `)`.

You might want to use regular expressions to parse an SGF text.
However, because game trees can be nested inside other game trees to any depth, standard regular expressions are not powerful enough to keep track of the nesting levels.

There are a couple of approaches to solving this exercise:

### State machine

You could scan the input string character-by-character, maintaining a variable holding your current position.
You can keep track of the current state of the parsed text based on the character at the current position.
The "current state" might be something like "reading a property name" or "inside a property value".
There are lots of values you need to keep track of with this approach.

### Parser

A [PEG-based parser][PEG-parser] is a very good way to parse a language that has a defined grammar.
[LPeg][LPeg] is a PEG parser, and it is one of the required dependencies of MoonScript, so you'll already have access to it.
A parser allows you to write rules for what the text should look like.
For example, "a property name is all uppercase letters", or "a property value is enclosed in brackets".
The parser then does the heavy lifting of matching the input text against your rules.

A couple of learning resources for LPeg:

* [LPeg tutorial][tutorial] in the lua-users wiki.
* [Mastering LPeg][mastering-lpeg] is written by one of the authors of LPeg and Lua itself. 
  He is a university professor and researcher, so this document reads like an academic paper.

[PEG-parser]: https://en.wikipedia.org/wiki/Parsing_expression_grammar
[LPeg]: https://www.inf.puc-rio.br/~roberto/lpeg/lpeg.html
[mastering-lpeg]: https://www.inf.puc-rio.br/~roberto/docs/lpeg-primer.pdf
[tutorial]: http://lua-users.org/wiki/LpegTutorial