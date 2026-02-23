-- Require the hello-world module
hello_world = require 'hello_world'

-- Define a module named hello-world. This module should return a single
-- function named hello that takes no arguments and returns a string.

describe 'hello-world', ->
  it 'says hello world', ->
    result = hello_world.hello!
    assert.are.equal 'Hello, World!', result
