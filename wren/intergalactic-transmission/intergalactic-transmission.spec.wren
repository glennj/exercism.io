import "./intergalactic-transmission" for IntergalacticTransmission
import "wren-testie/testie" for Testie, Expect

Testie.test("IntergalacticTransmission") { |do, skip|
  do.test("calculate transmit sequences -> empty message") {
    var message = [
    ]
    var expected = [
    ]
    Expect.value(IntergalacticTransmission.transmitSequence(message)).toEqual(expected)
  }

  do.test("calculate transmit sequences -> 0x00 is transmitted as 0x0000") {
    var message = [
      0x00,
    ]
    var expected = [
      0x00,
      0x00,
    ]
    Expect.value(IntergalacticTransmission.transmitSequence(message)).toEqual(expected)
  }

  do.test("calculate transmit sequences -> 0x02 is transmitted as 0x0300") {
    var message = [
      0x02,
    ]
    var expected = [
      0x03,
      0x00,
    ]
    Expect.value(IntergalacticTransmission.transmitSequence(message)).toEqual(expected)
  }

  do.test("calculate transmit sequences -> 0x06 is transmitted as 0x0600") {
    var message = [
      0x06,
    ]
    var expected = [
      0x06,
      0x00,
    ]
    Expect.value(IntergalacticTransmission.transmitSequence(message)).toEqual(expected)
  }

  do.test("calculate transmit sequences -> 0x05 is transmitted as 0x0581") {
    var message = [
      0x05,
    ]
    var expected = [
      0x05,
      0x81,
    ]
    Expect.value(IntergalacticTransmission.transmitSequence(message)).toEqual(expected)
  }

  do.test("calculate transmit sequences -> 0x29 is transmitted as 0x2881") {
    var message = [
      0x29,
    ]
    var expected = [
      0x28,
      0x81,
    ]
    Expect.value(IntergalacticTransmission.transmitSequence(message)).toEqual(expected)
  }

  do.test("calculate transmit sequences -> 0xc001c0de is transmitted as 0xc000711be1") {
    var message = [
      0xc0,
      0x01,
      0xc0,
      0xde,
    ]
    var expected = [
      0xc0,
      0x00,
      0x71,
      0x1b,
      0xe1,
    ]
    Expect.value(IntergalacticTransmission.transmitSequence(message)).toEqual(expected)
  }

  do.test("calculate transmit sequences -> six byte message") {
    var message = [
      0x47,
      0x72,
      0x65,
      0x61,
      0x74,
      0x21,
    ]
    var expected = [
      0x47,
      0xb8,
      0x99,
      0xac,
      0x17,
      0xa0,
      0x84,
    ]
    Expect.value(IntergalacticTransmission.transmitSequence(message)).toEqual(expected)
  }

  do.test("calculate transmit sequences -> seven byte message") {
    var message = [
      0x47,
      0x72,
      0x65,
      0x61,
      0x74,
      0x31,
      0x21,
    ]
    var expected = [
      0x47,
      0xb8,
      0x99,
      0xac,
      0x17,
      0xa0,
      0xc5,
      0x42,
    ]
    Expect.value(IntergalacticTransmission.transmitSequence(message)).toEqual(expected)
  }

  do.test("calculate transmit sequences -> eight byte message") {
    var message = [
      0xc0,
      0x01,
      0x13,
      0x37,
      0xc0,
      0xde,
      0x21,
      0x21,
    ]
    var expected = [
      0xc0,
      0x00,
      0x44,
      0x66,
      0x7d,
      0x06,
      0x78,
      0x42,
      0x21,
      0x81,
    ]
    Expect.value(IntergalacticTransmission.transmitSequence(message)).toEqual(expected)
  }

  do.test("calculate transmit sequences -> twenty byte message") {
    var message = [
      0x45,
      0x78,
      0x65,
      0x72,
      0x63,
      0x69,
      0x73,
      0x6d,
      0x20,
      0x69,
      0x73,
      0x20,
      0x61,
      0x77,
      0x65,
      0x73,
      0x6f,
      0x6d,
      0x65,
      0x21,
    ]
    var expected = [
      0x44,
      0xbd,
      0x18,
      0xaf,
      0x27,
      0x1b,
      0xa5,
      0xe7,
      0x6c,
      0x90,
      0x1b,
      0x2e,
      0x33,
      0x03,
      0x84,
      0xee,
      0x65,
      0xb8,
      0xdb,
      0xed,
      0xd7,
      0x28,
      0x84,
    ]
    Expect.value(IntergalacticTransmission.transmitSequence(message)).toEqual(expected)
  }

  do.test("decode received messages -> empty message") {
    var message = [
    ]
    var expected = [
    ]
    Expect.value(IntergalacticTransmission.decodeMessage(message)).toEqual(expected)
  }

  do.test("decode received messages -> zero message") {
    var message = [
      0x00,
      0x00,
    ]
    var expected = [
      0x00,
    ]
    Expect.value(IntergalacticTransmission.decodeMessage(message)).toEqual(expected)
  }

  do.test("decode received messages -> 0x0300 is decoded to 0x02") {
    var message = [
      0x03,
      0x00,
    ]
    var expected = [
      0x02,
    ]
    Expect.value(IntergalacticTransmission.decodeMessage(message)).toEqual(expected)
  }

  do.test("decode received messages -> 0x0581 is decoded to 0x05") {
    var message = [
      0x05,
      0x81,
    ]
    var expected = [
      0x05,
    ]
    Expect.value(IntergalacticTransmission.decodeMessage(message)).toEqual(expected)
  }

  do.test("decode received messages -> 0x2881 is decoded to 0x29") {
    var message = [
      0x28,
      0x81,
    ]
    var expected = [
      0x29,
    ]
    Expect.value(IntergalacticTransmission.decodeMessage(message)).toEqual(expected)
  }

  do.test("decode received messages -> first byte has wrong parity") {
    var message = [
      0x07,
      0x00,
    ]
    Expect.that {
      IntergalacticTransmission.decodeMessage(message)
    }.abortsWith("wrong parity")
  }

  do.test("decode received messages -> second byte has wrong parity") {
    var message = [
      0x03,
      0x68,
    ]
    Expect.that {
      IntergalacticTransmission.decodeMessage(message)
    }.abortsWith("wrong parity")
  }

  do.test("decode received messages -> 0xcf4b00 is decoded to 0xce94") {
    var message = [
      0xcf,
      0x4b,
      0x00,
    ]
    var expected = [
      0xce,
      0x94,
    ]
    Expect.value(IntergalacticTransmission.decodeMessage(message)).toEqual(expected)
  }

  do.test("decode received messages -> 0xe2566500 is decoded to 0xe2ad90") {
    var message = [
      0xe2,
      0x56,
      0x65,
      0x00,
    ]
    var expected = [
      0xe2,
      0xad,
      0x90,
    ]
    Expect.value(IntergalacticTransmission.decodeMessage(message)).toEqual(expected)
  }

  do.test("decode received messages -> six byte message") {
    var message = [
      0x47,
      0xb8,
      0x99,
      0xac,
      0x17,
      0xa0,
      0x84,
    ]
    var expected = [
      0x47,
      0x72,
      0x65,
      0x61,
      0x74,
      0x21,
    ]
    Expect.value(IntergalacticTransmission.decodeMessage(message)).toEqual(expected)
  }

  do.test("decode received messages -> seven byte message") {
    var message = [
      0x47,
      0xb8,
      0x99,
      0xac,
      0x17,
      0xa0,
      0xc5,
      0x42,
    ]
    var expected = [
      0x47,
      0x72,
      0x65,
      0x61,
      0x74,
      0x31,
      0x21,
    ]
    Expect.value(IntergalacticTransmission.decodeMessage(message)).toEqual(expected)
  }

  do.test("decode received messages -> last byte has wrong parity") {
    var message = [
      0x47,
      0xb8,
      0x99,
      0xac,
      0x17,
      0xa0,
      0xc5,
      0x43,
    ]
    Expect.that {
      IntergalacticTransmission.decodeMessage(message)
    }.abortsWith("wrong parity")
  }

  do.test("decode received messages -> eight byte message") {
    var message = [
      0xc0,
      0x00,
      0x44,
      0x66,
      0x7d,
      0x06,
      0x78,
      0x42,
      0x21,
      0x81,
    ]
    var expected = [
      0xc0,
      0x01,
      0x13,
      0x37,
      0xc0,
      0xde,
      0x21,
      0x21,
    ]
    Expect.value(IntergalacticTransmission.decodeMessage(message)).toEqual(expected)
  }

  do.test("decode received messages -> twenty byte message") {
    var message = [
      0x44,
      0xbd,
      0x18,
      0xaf,
      0x27,
      0x1b,
      0xa5,
      0xe7,
      0x6c,
      0x90,
      0x1b,
      0x2e,
      0x33,
      0x03,
      0x84,
      0xee,
      0x65,
      0xb8,
      0xdb,
      0xed,
      0xd7,
      0x28,
      0x84,
    ]
    var expected = [
      0x45,
      0x78,
      0x65,
      0x72,
      0x63,
      0x69,
      0x73,
      0x6d,
      0x20,
      0x69,
      0x73,
      0x20,
      0x61,
      0x77,
      0x65,
      0x73,
      0x6f,
      0x6d,
      0x65,
      0x21,
    ]
    Expect.value(IntergalacticTransmission.decodeMessage(message)).toEqual(expected)
  }

  do.test("decode received messages -> wrong parity on 16th byte") {
    var message = [
      0x44,
      0xbd,
      0x18,
      0xaf,
      0x27,
      0x1b,
      0xa5,
      0xe7,
      0x6c,
      0x90,
      0x1b,
      0x2e,
      0x33,
      0x03,
      0x84,
      0xef,
      0x65,
      0xb8,
      0xdb,
      0xed,
      0xd7,
      0x28,
      0x84,
    ]
    Expect.that {
      IntergalacticTransmission.decodeMessage(message)
    }.abortsWith("wrong parity")
  }
}
