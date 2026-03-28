Markdown = require 'markdown'

describe 'markdown', ->
  it 'parses normal text as a paragraph', ->
    result = Markdown.parse "This will be a paragraph"
    expected = '<p>This will be a paragraph</p>'
    assert.are.equal expected, result

  pending 'parsing italics', ->
    result = Markdown.parse "_This will be italic_"
    expected = '<p><em>This will be italic</em></p>'
    assert.are.equal expected, result

  pending 'parsing bold text', ->
    result = Markdown.parse "__This will be bold__"
    expected = '<p><strong>This will be bold</strong></p>'
    assert.are.equal expected, result

  pending 'mixed normal, italics and bold text', ->
    result = Markdown.parse "This will _be_ __mixed__"
    expected = '<p>This will <em>be</em> <strong>mixed</strong></p>'
    assert.are.equal expected, result

  pending 'with h1 header level', ->
    result = Markdown.parse "# This will be an h1"
    expected = '<h1>This will be an h1</h1>'
    assert.are.equal expected, result

  pending 'with h2 header level', ->
    result = Markdown.parse "## This will be an h2"
    expected = '<h2>This will be an h2</h2>'
    assert.are.equal expected, result

  pending 'with h3 header level', ->
    result = Markdown.parse "### This will be an h3"
    expected = '<h3>This will be an h3</h3>'
    assert.are.equal expected, result

  pending 'with h4 header level', ->
    result = Markdown.parse "#### This will be an h4"
    expected = '<h4>This will be an h4</h4>'
    assert.are.equal expected, result

  pending 'with h5 header level', ->
    result = Markdown.parse "##### This will be an h5"
    expected = '<h5>This will be an h5</h5>'
    assert.are.equal expected, result

  pending 'with h6 header level', ->
    result = Markdown.parse "###### This will be an h6"
    expected = '<h6>This will be an h6</h6>'
    assert.are.equal expected, result

  pending 'h7 header level is a paragraph', ->
    result = Markdown.parse "####### This will not be an h7"
    expected = '<p>####### This will not be an h7</p>'
    assert.are.equal expected, result

  pending 'unordered lists', ->
    result = Markdown.parse "* Item 1\n* Item 2"
    expected = '<ul><li>Item 1</li><li>Item 2</li></ul>'
    assert.are.equal expected, result

  pending 'With a little bit of everything', ->
    result = Markdown.parse "# Header!\n* __Bold Item__\n* _Italic Item_"
    expected = '<h1>Header!</h1><ul><li><strong>Bold Item</strong></li><li><em>Italic Item</em></li></ul>'
    assert.are.equal expected, result

  pending 'with markdown symbols in the header text that should not be interpreted', ->
    result = Markdown.parse "# This is a header with # and * in the text"
    expected = '<h1>This is a header with # and * in the text</h1>'
    assert.are.equal expected, result

  pending 'with markdown symbols in the list item text that should not be interpreted', ->
    result = Markdown.parse "* Item 1 with a # in the text\n* Item 2 with * in the text"
    expected = '<ul><li>Item 1 with a # in the text</li><li>Item 2 with * in the text</li></ul>'
    assert.are.equal expected, result

  pending 'with markdown symbols in the paragraph text that should not be interpreted', ->
    result = Markdown.parse "This is a paragraph with # and * in the text"
    expected = '<p>This is a paragraph with # and * in the text</p>'
    assert.are.equal expected, result

  pending 'unordered lists close properly with preceding and following lines', ->
    result = Markdown.parse "# Start a list\n* Item 1\n* Item 2\nEnd a list"
    expected = '<h1>Start a list</h1><ul><li>Item 1</li><li>Item 2</li></ul><p>End a list</p>'
    assert.are.equal expected, result
