#!/usr/bin/env tclsh
# generated: 2026-07-18T02:13:41Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "markdown.tcl"


test markdown-1 "parses normal text as a paragraph" -body {
    parse "This will be a paragraph"
} -returnCodes ok -result "<p>This will be a paragraph</p>"

skip markdown-2
test markdown-2 "parsing italics" -body {
    parse "_This will be italic_"
} -returnCodes ok -result "<p><em>This will be italic</em></p>"

skip markdown-3
test markdown-3 "parsing bold text" -body {
    parse "__This will be bold__"
} -returnCodes ok -result "<p><strong>This will be bold</strong></p>"

skip markdown-4
test markdown-4 "mixed normal, italics and bold text" -body {
    parse "This will _be_ __mixed__"
} -returnCodes ok -result "<p>This will <em>be</em> <strong>mixed</strong></p>"

skip markdown-5
test markdown-5 "with h1 header level" -body {
    parse "# This will be an h1"
} -returnCodes ok -result "<h1>This will be an h1</h1>"

skip markdown-6
test markdown-6 "with h2 header level" -body {
    parse "## This will be an h2"
} -returnCodes ok -result "<h2>This will be an h2</h2>"

skip markdown-7
test markdown-7 "with h3 header level" -body {
    parse "### This will be an h3"
} -returnCodes ok -result "<h3>This will be an h3</h3>"

skip markdown-8
test markdown-8 "with h4 header level" -body {
    parse "#### This will be an h4"
} -returnCodes ok -result "<h4>This will be an h4</h4>"

skip markdown-9
test markdown-9 "with h5 header level" -body {
    parse "##### This will be an h5"
} -returnCodes ok -result "<h5>This will be an h5</h5>"

skip markdown-10
test markdown-10 "with h6 header level" -body {
    parse "###### This will be an h6"
} -returnCodes ok -result "<h6>This will be an h6</h6>"

skip markdown-11
test markdown-11 "h7 header level is a paragraph" -body {
    parse "####### This will not be an h7"
} -returnCodes ok -result "<p>####### This will not be an h7</p>"

skip markdown-12
test markdown-12 "unordered lists" -body {
    parse "* Item 1\n* Item 2"
} -returnCodes ok -result "<ul><li>Item 1</li><li>Item 2</li></ul>"

skip markdown-13
test markdown-13 "With a little bit of everything" -body {
    parse "# Header!\n* __Bold Item__\n* _Italic Item_"
} -returnCodes ok -result "<h1>Header!</h1><ul><li><strong>Bold Item</strong></li><li><em>Italic Item</em></li></ul>"

skip markdown-14
test markdown-14 "with markdown symbols in the header text that should not be interpreted" -body {
    parse "# This is a header with # and * in the text"
} -returnCodes ok -result "<h1>This is a header with # and * in the text</h1>"

skip markdown-15
test markdown-15 "with markdown symbols in the list item text that should not be interpreted" -body {
    parse "* Item 1 with a # in the text\n* Item 2 with * in the text"
} -returnCodes ok -result "<ul><li>Item 1 with a # in the text</li><li>Item 2 with * in the text</li></ul>"

skip markdown-16
test markdown-16 "with markdown symbols in the paragraph text that should not be interpreted" -body {
    parse "This is a paragraph with # and * in the text"
} -returnCodes ok -result "<p>This is a paragraph with # and * in the text</p>"

skip markdown-17
test markdown-17 "unordered lists close properly with preceding and following lines" -body {
    parse "# Start a list\n* Item 1\n* Item 2\nEnd a list"
} -returnCodes ok -result "<h1>Start a list</h1><ul><li>Item 1</li><li>Item 2</li></ul><p>End a list</p>"

skip markdown-18
test markdown-18 "multiple em on a line" -body {
    parse "foo _bar_ _baz_ quz"
} -returnCodes ok -result "<p>foo <em>bar</em> <em>baz</em> quz</p>"

skip markdown-19
test markdown-19 "multiple strong on a line" -body {
    parse "foo __bar__ __baz__ quz"
} -returnCodes ok -result "<p>foo <strong>bar</strong> <strong>baz</strong> quz</p>"

cleanupTests
