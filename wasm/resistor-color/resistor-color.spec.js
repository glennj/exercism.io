import { compileWat, WasmRunner } from "@exercism/wasm-lib";

let wasmModule;
let currentInstance;

beforeAll(async () => {
  try {
    const watPath = new URL("./resistor-color.wat", import.meta.url);
    const { buffer } = await compileWat(watPath);
    wasmModule = await WebAssembly.compile(buffer);
  } catch (err) {
    console.log(`Error compiling *.wat: \n${err}`);
    process.exit(1);
  }
});

function colorCode(input = "") {
  const inputBufferOffset = 64;
  const inputBufferCapacity = 128;

  const inputLengthEncoded = new TextEncoder().encode(input).length;
  if (inputLengthEncoded > inputBufferCapacity) {
    throw new Error(
      `String is too large for buffer of size ${inputBufferCapacity} bytes`
    );
  }

  currentInstance.set_mem_as_utf8(inputBufferOffset, inputLengthEncoded, input);

  return currentInstance.exports.colorCode(
    inputBufferOffset,
    inputLengthEncoded
  );
}

describe("ResistorColor", () => {
  beforeEach(async () => {
    currentInstance = null;

    if (!wasmModule) {
      return Promise.reject();
    }
    try {
      currentInstance = await new WasmRunner(wasmModule);
      return Promise.resolve();
    } catch (err) {
      console.log(`Error instantiating WebAssembly module: ${err}`);
      return Promise.reject();
    }
  });

  describe("Color codes", () => {
    test("Black", () => {
      expect(colorCode("black")).toEqual(0);
    });

    test("brown", () => {
      expect(colorCode("brown")).toEqual(1);
    });

    test("red", () => {
      expect(colorCode("red")).toEqual(2);
    });

    test("orange", () => {
      expect(colorCode("orange")).toEqual(3);
    });

    test("yellow", () => {
      expect(colorCode("yellow")).toEqual(4);
    });

    test("green", () => {
      expect(colorCode("green")).toEqual(5);
    });

    test("blue", () => {
      expect(colorCode("blue")).toEqual(6);
    });

    test("violet", () => {
      expect(colorCode("violet")).toEqual(7);
    });

    test("grey", () => {
      expect(colorCode("grey")).toEqual(8);
    });

    test("White", () => {
      expect(colorCode("white")).toEqual(9);
    });

    test("Orange", () => {
      expect(colorCode("orange")).toEqual(3);
    });

    test("Invalid color", () => {
      expect(colorCode("pink")).toEqual(-1);
    });
  });

  test("Colors", () => {
    const [offset, length] = currentInstance.exports.colors();
    const commaDelimited = currentInstance.get_mem_as_utf8(offset, length);
    const colors = commaDelimited.split(",");

    expect(colors).toEqual([
      "black",
      "brown",
      "red",
      "orange",
      "yellow",
      "green",
      "blue",
      "violet",
      "grey",
      "white",
    ]);
  });
});
