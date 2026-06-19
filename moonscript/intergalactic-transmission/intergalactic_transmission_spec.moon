import transmitSequence, decodeMessage from require 'intergalactic_transmission'

describe 'intergalactic-transmission:', ->
  -- Inputs and expected data are given in hexadecimal.

  describe 'calculate transmit sequences:', ->
    it 'empty message', ->
      result = transmitSequence {}
      expected = {}
      assert.are.same expected, result

    it '0x00 is transmitted as 0x0000', ->
      result = transmitSequence {'0x00'}
      expected = {'0x00', '0x00'}
      assert.are.same expected, result

    it '0x02 is transmitted as 0x0300', ->
      result = transmitSequence {'0x02'}
      expected = {'0x03', '0x00'}
      assert.are.same expected, result

    it '0x06 is transmitted as 0x0600', ->
      result = transmitSequence {'0x06'}
      expected = {'0x06', '0x00'}
      assert.are.same expected, result

    it '0x05 is transmitted as 0x0581', ->
      result = transmitSequence {'0x05'}
      expected = {'0x05', '0x81'}
      assert.are.same expected, result

    it '0x29 is transmitted as 0x2881', ->
      result = transmitSequence {'0x29'}
      expected = {'0x28', '0x81'}
      assert.are.same expected, result

    it '0xc001c0de is transmitted as 0xc000711be1', ->
      result = transmitSequence {'0xc0', '0x01', '0xc0', '0xde'}
      expected = {'0xc0', '0x00', '0x71', '0x1b', '0xe1'}
      assert.are.same expected, result

    it 'six byte message', ->
      result = transmitSequence {'0x47', '0x72', '0x65', '0x61', '0x74', '0x21'}
      expected = {'0x47', '0xb8', '0x99', '0xac', '0x17', '0xa0', '0x84'}
      assert.are.same expected, result

    it 'seven byte message', ->
      result = transmitSequence {'0x47', '0x72', '0x65', '0x61', '0x74', '0x31', '0x21'}
      expected = {
        '0x47',
        '0xb8',
        '0x99',
        '0xac',
        '0x17',
        '0xa0',
        '0xc5',
        '0x42',
      }
      assert.are.same expected, result

    it 'eight byte message', ->
      result = transmitSequence {
        '0xc0',
        '0x01',
        '0x13',
        '0x37',
        '0xc0',
        '0xde',
        '0x21',
        '0x21',
      }
      expected = {
        '0xc0',
        '0x00',
        '0x44',
        '0x66',
        '0x7d',
        '0x06',
        '0x78',
        '0x42',
        '0x21',
        '0x81',
      }
      assert.are.same expected, result

    it 'twenty byte message', ->
      result = transmitSequence {
        '0x45',
        '0x78',
        '0x65',
        '0x72',
        '0x63',
        '0x69',
        '0x73',
        '0x6d',
        '0x20',
        '0x69',
        '0x73',
        '0x20',
        '0x61',
        '0x77',
        '0x65',
        '0x73',
        '0x6f',
        '0x6d',
        '0x65',
        '0x21',
      }
      expected = {
        '0x44',
        '0xbd',
        '0x18',
        '0xaf',
        '0x27',
        '0x1b',
        '0xa5',
        '0xe7',
        '0x6c',
        '0x90',
        '0x1b',
        '0x2e',
        '0x33',
        '0x03',
        '0x84',
        '0xee',
        '0x65',
        '0xb8',
        '0xdb',
        '0xed',
        '0xd7',
        '0x28',
        '0x84',
      }
      assert.are.same expected, result

  describe 'decode received messages:', ->
    it 'empty message', ->
      result = decodeMessage {}
      expected = {}
      assert.are.same expected, result

    it 'zero message', ->
      result = decodeMessage {'0x00', '0x00'}
      expected = {'0x00'}
      assert.are.same expected, result

    it '0x0300 is decoded to 0x02', ->
      result = decodeMessage {'0x03', '0x00'}
      expected = {'0x02'}
      assert.are.same expected, result

    it '0x0581 is decoded to 0x05', ->
      result = decodeMessage {'0x05', '0x81'}
      expected = {'0x05'}
      assert.are.same expected, result

    it '0x2881 is decoded to 0x29', ->
      result = decodeMessage {'0x28', '0x81'}
      expected = {'0x29'}
      assert.are.same expected, result

    it 'first byte has wrong parity', ->
      f = -> decodeMessage {'0x07', '0x00'}
      assert.has.error f, 'wrong parity'

    it 'second byte has wrong parity', ->
      f = -> decodeMessage {'0x03', '0x68'}
      assert.has.error f, 'wrong parity'

    it '0xcf4b00 is decoded to 0xce94', ->
      result = decodeMessage {'0xcf', '0x4b', '0x00'}
      expected = {'0xce', '0x94'}
      assert.are.same expected, result

    it '0xe2566500 is decoded to 0xe2ad90', ->
      result = decodeMessage {'0xe2', '0x56', '0x65', '0x00'}
      expected = {'0xe2', '0xad', '0x90'}
      assert.are.same expected, result

    it 'six byte message', ->
      result = decodeMessage {'0x47', '0xb8', '0x99', '0xac', '0x17', '0xa0', '0x84'}
      expected = {'0x47', '0x72', '0x65', '0x61', '0x74', '0x21'}
      assert.are.same expected, result

    it 'seven byte message', ->
      result = decodeMessage {
        '0x47',
        '0xb8',
        '0x99',
        '0xac',
        '0x17',
        '0xa0',
        '0xc5',
        '0x42',
      }
      expected = {'0x47', '0x72', '0x65', '0x61', '0x74', '0x31', '0x21'}
      assert.are.same expected, result

    it 'last byte has wrong parity', ->
      f = -> decodeMessage {
        '0x47',
        '0xb8',
        '0x99',
        '0xac',
        '0x17',
        '0xa0',
        '0xc5',
        '0x43',
      }
      assert.has.error f, 'wrong parity'

    it 'eight byte message', ->
      result = decodeMessage {
        '0xc0',
        '0x00',
        '0x44',
        '0x66',
        '0x7d',
        '0x06',
        '0x78',
        '0x42',
        '0x21',
        '0x81',
      }
      expected = {
        '0xc0',
        '0x01',
        '0x13',
        '0x37',
        '0xc0',
        '0xde',
        '0x21',
        '0x21',
      }
      assert.are.same expected, result

    it 'twenty byte message', ->
      result = decodeMessage {
        '0x44',
        '0xbd',
        '0x18',
        '0xaf',
        '0x27',
        '0x1b',
        '0xa5',
        '0xe7',
        '0x6c',
        '0x90',
        '0x1b',
        '0x2e',
        '0x33',
        '0x03',
        '0x84',
        '0xee',
        '0x65',
        '0xb8',
        '0xdb',
        '0xed',
        '0xd7',
        '0x28',
        '0x84',
      }
      expected = {
        '0x45',
        '0x78',
        '0x65',
        '0x72',
        '0x63',
        '0x69',
        '0x73',
        '0x6d',
        '0x20',
        '0x69',
        '0x73',
        '0x20',
        '0x61',
        '0x77',
        '0x65',
        '0x73',
        '0x6f',
        '0x6d',
        '0x65',
        '0x21',
      }
      assert.are.same expected, result

    it 'wrong parity on 16th byte', ->
      f = -> decodeMessage {
        '0x44',
        '0xbd',
        '0x18',
        '0xaf',
        '0x27',
        '0x1b',
        '0xa5',
        '0xe7',
        '0x6c',
        '0x90',
        '0x1b',
        '0x2e',
        '0x33',
        '0x03',
        '0x84',
        '0xef',
        '0x65',
        '0xb8',
        '0xdb',
        '0xed',
        '0xd7',
        '0x28',
        '0x84',
      }
      assert.has.error f, 'wrong parity'
