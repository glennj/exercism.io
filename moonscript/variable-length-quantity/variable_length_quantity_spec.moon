VariableLengthQuantity = require 'variable_length_quantity'

describe 'variable-length-quantity', ->
  describe 'Encode a series of integers, producing a series of bytes.', ->
    it 'zero', ->
      result = VariableLengthQuantity.encode {0}
      expected = {0}
      assert.are.same expected, result

    pending 'arbitrary single byte', ->
      result = VariableLengthQuantity.encode {64}
      expected = {64}
      assert.are.same expected, result

    pending 'asymmetric single byte', ->
      result = VariableLengthQuantity.encode {83}
      expected = {83}
      assert.are.same expected, result

    pending 'largest single byte', ->
      result = VariableLengthQuantity.encode {127}
      expected = {127}
      assert.are.same expected, result

    pending 'smallest double byte', ->
      result = VariableLengthQuantity.encode {128}
      expected = {129, 0}
      assert.are.same expected, result

    pending 'arbitrary double byte', ->
      result = VariableLengthQuantity.encode {8192}
      expected = {192, 0}
      assert.are.same expected, result

    pending 'asymmetric double byte', ->
      result = VariableLengthQuantity.encode {173}
      expected = {129, 45}
      assert.are.same expected, result

    pending 'largest double byte', ->
      result = VariableLengthQuantity.encode {16383}
      expected = {255, 127}
      assert.are.same expected, result

    pending 'smallest triple byte', ->
      result = VariableLengthQuantity.encode {16384}
      expected = {129, 128, 0}
      assert.are.same expected, result

    pending 'arbitrary triple byte', ->
      result = VariableLengthQuantity.encode {1048576}
      expected = {192, 128, 0}
      assert.are.same expected, result

    pending 'asymmetric triple byte', ->
      result = VariableLengthQuantity.encode {120220}
      expected = {135, 171, 28}
      assert.are.same expected, result

    pending 'largest triple byte', ->
      result = VariableLengthQuantity.encode {2097151}
      expected = {255, 255, 127}
      assert.are.same expected, result

    pending 'smallest quadruple byte', ->
      result = VariableLengthQuantity.encode {2097152}
      expected = {129, 128, 128, 0}
      assert.are.same expected, result

    pending 'arbitrary quadruple byte', ->
      result = VariableLengthQuantity.encode {134217728}
      expected = {192, 128, 128, 0}
      assert.are.same expected, result

    pending 'asymmetric quadruple byte', ->
      result = VariableLengthQuantity.encode {3503876}
      expected = {129, 213, 238, 4}
      assert.are.same expected, result

    pending 'largest quadruple byte', ->
      result = VariableLengthQuantity.encode {268435455}
      expected = {255, 255, 255, 127}
      assert.are.same expected, result

    pending 'smallest quintuple byte', ->
      result = VariableLengthQuantity.encode {268435456}
      expected = {129, 128, 128, 128, 0}
      assert.are.same expected, result

    pending 'arbitrary quintuple byte', ->
      result = VariableLengthQuantity.encode {4278190080}
      expected = {143, 248, 128, 128, 0}
      assert.are.same expected, result

    pending 'asymmetric quintuple byte', ->
      result = VariableLengthQuantity.encode {2254790917}
      expected = {136, 179, 149, 194, 5}
      assert.are.same expected, result

    pending 'maximum 32-bit integer input', ->
      result = VariableLengthQuantity.encode {4294967295}
      expected = {143, 255, 255, 255, 127}
      assert.are.same expected, result

    pending 'two single-byte values', ->
      result = VariableLengthQuantity.encode {64, 127}
      expected = {64, 127}
      assert.are.same expected, result

    pending 'two multi-byte values', ->
      result = VariableLengthQuantity.encode {16384, 1193046}
      expected = {129, 128, 0, 200, 232, 86}
      assert.are.same expected, result

    pending 'many multi-byte values', ->
      result = VariableLengthQuantity.encode {8192, 1193046, 268435455, 0, 16383, 16384}
      expected = {192, 0, 200, 232, 86, 255, 255, 255, 127, 0, 255, 127, 129, 128, 0}
      assert.are.same expected, result

  describe 'Decode a series of bytes, producing a series of integers.', ->
    pending 'one byte', ->
      result = VariableLengthQuantity.decode {127}
      expected = {127}
      assert.are.same expected, result

    pending 'two bytes', ->
      result = VariableLengthQuantity.decode {192, 0}
      expected = {8192}
      assert.are.same expected, result

    pending 'three bytes', ->
      result = VariableLengthQuantity.decode {255, 255, 127}
      expected = {2097151}
      assert.are.same expected, result

    pending 'four bytes', ->
      result = VariableLengthQuantity.decode {129, 128, 128, 0}
      expected = {2097152}
      assert.are.same expected, result

    pending 'maximum 32-bit integer', ->
      result = VariableLengthQuantity.decode {143, 255, 255, 255, 127}
      expected = {4294967295}
      assert.are.same expected, result

    pending 'incomplete sequence causes error', ->
      f = -> VariableLengthQuantity.decode {255}
      assert.has.errors f, 'incomplete sequence'

    pending 'incomplete sequence causes error, even if value is zero', ->
      f = -> VariableLengthQuantity.decode {128}
      assert.has.errors f, 'incomplete sequence'

    pending 'multiple values', ->
      result = VariableLengthQuantity.decode {192, 0, 200, 232, 86, 255, 255, 255, 127, 0, 255, 127, 129, 128, 0}
      expected = {8192, 1193046, 268435455, 0, 16383, 16384}
      assert.are.same expected, result
