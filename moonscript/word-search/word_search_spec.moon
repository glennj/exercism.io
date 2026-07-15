WordSearch = require 'word_search'

assert\set_parameter "TableFormatLevel", 3

describe 'word-search:', ->
  it 'Should accept an initial game grid and a target search word', ->
    grid = {'jefblpepre'}
    words = {'clojure'}
    expected = {}
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate one word written left to right', ->
    grid = {'clojurermt'}
    words = {'clojure'}
    expected = {
      clojure: {
        end: {
          column: 7
          row: 1
        }
        start: {
          column: 1
          row: 1
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate the same word written left to right in a different position', ->
    grid = {'mtclojurer'}
    words = {'clojure'}
    expected = {
      clojure: {
        end: {
          column: 9
          row: 1
        }
        start: {
          column: 3
          row: 1
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate a different left to right word', ->
    grid = {'coffeelplx'}
    words = {'coffee'}
    expected = {
      coffee: {
        end: {
          column: 6
          row: 1
        }
        start: {
          column: 1
          row: 1
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate that different left to right word in a different position', ->
    grid = {'xcoffeezlp'}
    words = {'coffee'}
    expected = {
      coffee: {
        end: {
          column: 7
          row: 1
        }
        start: {
          column: 2
          row: 1
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate a left to right word in two line grid', ->
    grid = {
      'jefblpepre',
      'tclojurerm',
    }
    words = {'clojure'}
    expected = {
      clojure: {
        end: {
          column: 8
          row: 2
        }
        start: {
          column: 2
          row: 2
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate a left to right word in three line grid', ->
    grid = {
      'camdcimgtc',
      'jefblpepre',
      'clojurermt',
    }
    words = {'clojure'}
    expected = {
      clojure: {
        end: {
          column: 7
          row: 3
        }
        start: {
          column: 1
          row: 3
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate a left to right word in ten line grid', ->
    grid = {
      'jefblpepre',
      'camdcimgtc',
      'oivokprjsm',
      'pbwasqroua',
      'rixilelhrs',
      'wolcqlirpc',
      'screeaumgr',
      'alxhpburyi',
      'jalaycalmp',
      'clojurermt',
    }
    words = {'clojure'}
    expected = {
      clojure: {
        end: {
          column: 7
          row: 10
        }
        start: {
          column: 1
          row: 10
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate that left to right word in a different position in a ten line grid', ->
    grid = {
      'jefblpepre',
      'camdcimgtc',
      'oivokprjsm',
      'pbwasqroua',
      'rixilelhrs',
      'wolcqlirpc',
      'screeaumgr',
      'alxhpburyi',
      'clojurermt',
      'jalaycalmp',
    }
    words = {'clojure'}
    expected = {
      clojure: {
        end: {
          column: 7
          row: 9
        }
        start: {
          column: 1
          row: 9
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate a different left to right word in a ten line grid', ->
    grid = {
      'jefblpepre',
      'camdcimgtc',
      'oivokprjsm',
      'pbwasqroua',
      'rixilelhrs',
      'wolcqlirpc',
      'fortranftw',
      'alxhpburyi',
      'clojurermt',
      'jalaycalmp',
    }
    words = {'fortran'}
    expected = {
      fortran: {
        end: {
          column: 7
          row: 7
        }
        start: {
          column: 1
          row: 7
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate multiple words', ->
    grid = {
      'jefblpepre',
      'camdcimgtc',
      'oivokprjsm',
      'pbwasqroua',
      'rixilelhrs',
      'wolcqlirpc',
      'fortranftw',
      'alxhpburyi',
      'jalaycalmp',
      'clojurermt',
    }
    words = {
      'fortran',
      'clojure',
    }
    expected = {
      clojure: {
        end: {
          column: 7
          row: 10
        }
        start: {
          column: 1
          row: 10
        }
      }
      fortran: {
        end: {
          column: 7
          row: 7
        }
        start: {
          column: 1
          row: 7
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate a single word written right to left', ->
    grid = {'rixilelhrs'}
    words = {'elixir'}
    expected = {
      elixir: {
        end: {
          column: 1
          row: 1
        }
        start: {
          column: 6
          row: 1
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate multiple words written in different horizontal directions', ->
    grid = {
      'jefblpepre',
      'camdcimgtc',
      'oivokprjsm',
      'pbwasqroua',
      'rixilelhrs',
      'wolcqlirpc',
      'screeaumgr',
      'alxhpburyi',
      'jalaycalmp',
      'clojurermt',
    }
    words = {
      'elixir',
      'clojure',
    }
    expected = {
      clojure: {
        end: {
          column: 7
          row: 10
        }
        start: {
          column: 1
          row: 10
        }
      }
      elixir: {
        end: {
          column: 1
          row: 5
        }
        start: {
          column: 6
          row: 5
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate words written top to bottom', ->
    grid = {
      'jefblpepre',
      'camdcimgtc',
      'oivokprjsm',
      'pbwasqroua',
      'rixilelhrs',
      'wolcqlirpc',
      'screeaumgr',
      'alxhpburyi',
      'jalaycalmp',
      'clojurermt',
    }
    words = {
      'clojure',
      'elixir',
      'ecmascript',
    }
    expected = {
      clojure: {
        end: {
          column: 7
          row: 10
        }
        start: {
          column: 1
          row: 10
        }
      }
      ecmascript: {
        end: {
          column: 10
          row: 10
        }
        start: {
          column: 10
          row: 1
        }
      }
      elixir: {
        end: {
          column: 1
          row: 5
        }
        start: {
          column: 6
          row: 5
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate words written bottom to top', ->
    grid = {
      'jefblpepre',
      'camdcimgtc',
      'oivokprjsm',
      'pbwasqroua',
      'rixilelhrs',
      'wolcqlirpc',
      'screeaumgr',
      'alxhpburyi',
      'jalaycalmp',
      'clojurermt',
    }
    words = {
      'clojure',
      'elixir',
      'ecmascript',
      'rust',
    }
    expected = {
      clojure: {
        end: {
          column: 7
          row: 10
        }
        start: {
          column: 1
          row: 10
        }
      }
      ecmascript: {
        end: {
          column: 10
          row: 10
        }
        start: {
          column: 10
          row: 1
        }
      }
      elixir: {
        end: {
          column: 1
          row: 5
        }
        start: {
          column: 6
          row: 5
        }
      }
      rust: {
        end: {
          column: 9
          row: 2
        }
        start: {
          column: 9
          row: 5
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate words written top left to bottom right', ->
    grid = {
      'jefblpepre',
      'camdcimgtc',
      'oivokprjsm',
      'pbwasqroua',
      'rixilelhrs',
      'wolcqlirpc',
      'screeaumgr',
      'alxhpburyi',
      'jalaycalmp',
      'clojurermt',
    }
    words = {
      'clojure',
      'elixir',
      'ecmascript',
      'rust',
      'java',
    }
    expected = {
      clojure: {
        end: {
          column: 7
          row: 10
        }
        start: {
          column: 1
          row: 10
        }
      }
      ecmascript: {
        end: {
          column: 10
          row: 10
        }
        start: {
          column: 10
          row: 1
        }
      }
      elixir: {
        end: {
          column: 1
          row: 5
        }
        start: {
          column: 6
          row: 5
        }
      }
      java: {
        end: {
          column: 4
          row: 4
        }
        start: {
          column: 1
          row: 1
        }
      }
      rust: {
        end: {
          column: 9
          row: 2
        }
        start: {
          column: 9
          row: 5
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate words written bottom right to top left', ->
    grid = {
      'jefblpepre',
      'camdcimgtc',
      'oivokprjsm',
      'pbwasqroua',
      'rixilelhrs',
      'wolcqlirpc',
      'screeaumgr',
      'alxhpburyi',
      'jalaycalmp',
      'clojurermt',
    }
    words = {
      'clojure',
      'elixir',
      'ecmascript',
      'rust',
      'java',
      'lua',
    }
    expected = {
      clojure: {
        end: {
          column: 7
          row: 10
        }
        start: {
          column: 1
          row: 10
        }
      }
      ecmascript: {
        end: {
          column: 10
          row: 10
        }
        start: {
          column: 10
          row: 1
        }
      }
      elixir: {
        end: {
          column: 1
          row: 5
        }
        start: {
          column: 6
          row: 5
        }
      }
      java: {
        end: {
          column: 4
          row: 4
        }
        start: {
          column: 1
          row: 1
        }
      }
      lua: {
        end: {
          column: 6
          row: 7
        }
        start: {
          column: 8
          row: 9
        }
      }
      rust: {
        end: {
          column: 9
          row: 2
        }
        start: {
          column: 9
          row: 5
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate words written bottom left to top right', ->
    grid = {
      'jefblpepre',
      'camdcimgtc',
      'oivokprjsm',
      'pbwasqroua',
      'rixilelhrs',
      'wolcqlirpc',
      'screeaumgr',
      'alxhpburyi',
      'jalaycalmp',
      'clojurermt',
    }
    words = {
      'clojure',
      'elixir',
      'ecmascript',
      'rust',
      'java',
      'lua',
      'lisp',
    }
    expected = {
      clojure: {
        end: {
          column: 7
          row: 10
        }
        start: {
          column: 1
          row: 10
        }
      }
      ecmascript: {
        end: {
          column: 10
          row: 10
        }
        start: {
          column: 10
          row: 1
        }
      }
      elixir: {
        end: {
          column: 1
          row: 5
        }
        start: {
          column: 6
          row: 5
        }
      }
      java: {
        end: {
          column: 4
          row: 4
        }
        start: {
          column: 1
          row: 1
        }
      }
      lisp: {
        end: {
          column: 6
          row: 3
        }
        start: {
          column: 3
          row: 6
        }
      }
      lua: {
        end: {
          column: 6
          row: 7
        }
        start: {
          column: 8
          row: 9
        }
      }
      rust: {
        end: {
          column: 9
          row: 2
        }
        start: {
          column: 9
          row: 5
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should locate words written top right to bottom left', ->
    grid = {
      'jefblpepre',
      'camdcimgtc',
      'oivokprjsm',
      'pbwasqroua',
      'rixilelhrs',
      'wolcqlirpc',
      'screeaumgr',
      'alxhpburyi',
      'jalaycalmp',
      'clojurermt',
    }
    words = {
      'clojure',
      'elixir',
      'ecmascript',
      'rust',
      'java',
      'lua',
      'lisp',
      'ruby',
    }
    expected = {
      clojure: {
        end: {
          column: 7
          row: 10
        }
        start: {
          column: 1
          row: 10
        }
      }
      ecmascript: {
        end: {
          column: 10
          row: 10
        }
        start: {
          column: 10
          row: 1
        }
      }
      elixir: {
        end: {
          column: 1
          row: 5
        }
        start: {
          column: 6
          row: 5
        }
      }
      java: {
        end: {
          column: 4
          row: 4
        }
        start: {
          column: 1
          row: 1
        }
      }
      lisp: {
        end: {
          column: 6
          row: 3
        }
        start: {
          column: 3
          row: 6
        }
      }
      lua: {
        end: {
          column: 6
          row: 7
        }
        start: {
          column: 8
          row: 9
        }
      }
      ruby: {
        end: {
          column: 5
          row: 9
        }
        start: {
          column: 8
          row: 6
        }
      }
      rust: {
        end: {
          column: 9
          row: 2
        }
        start: {
          column: 9
          row: 5
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should fail to locate a word that is not in the puzzle', ->
    grid = {
      'jefblpepre',
      'camdcimgtc',
      'oivokprjsm',
      'pbwasqroua',
      'rixilelhrs',
      'wolcqlirpc',
      'screeaumgr',
      'alxhpburyi',
      'jalaycalmp',
      'clojurermt',
    }
    words = {
      'clojure',
      'elixir',
      'ecmascript',
      'rust',
      'java',
      'lua',
      'lisp',
      'ruby',
      'haskell',
    }
    expected = {
      clojure: {
        end: {
          column: 7
          row: 10
        }
        start: {
          column: 1
          row: 10
        }
      }
      ecmascript: {
        end: {
          column: 10
          row: 10
        }
        start: {
          column: 10
          row: 1
        }
      }
      elixir: {
        end: {
          column: 1
          row: 5
        }
        start: {
          column: 6
          row: 5
        }
      }
      java: {
        end: {
          column: 4
          row: 4
        }
        start: {
          column: 1
          row: 1
        }
      }
      lisp: {
        end: {
          column: 6
          row: 3
        }
        start: {
          column: 3
          row: 6
        }
      }
      lua: {
        end: {
          column: 6
          row: 7
        }
        start: {
          column: 8
          row: 9
        }
      }
      ruby: {
        end: {
          column: 5
          row: 9
        }
        start: {
          column: 8
          row: 6
        }
      }
      rust: {
        end: {
          column: 9
          row: 2
        }
        start: {
          column: 9
          row: 5
        }
      }
    }
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should fail to locate words that are not on horizontal, vertical, or diagonal lines', ->
    grid = {
      'abc',
      'def',
    }
    words = {
      'aef',
      'ced',
      'abf',
      'cbd',
    }
    expected = {}
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should not concatenate different lines to find a horizontal word', ->
    grid = {
      'abceli',
      'xirdfg',
    }
    words = {'elixir'}
    expected = {}
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should not wrap around horizontally to find a word', ->
    grid = {'silabcdefp'}
    words = {'lisp'}
    expected = {}
    assert.are.same expected, WordSearch(grid)\find(words)

  it 'Should not wrap around vertically to find a word', ->
    grid = {
      's',
      'u',
      'r',
      'a',
      'b',
      'c',
      't',
    }
    words = {'rust'}
    expected = {}
    assert.are.same expected, WordSearch(grid)\find(words)

