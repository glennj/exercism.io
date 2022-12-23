import "wren-testie/testie" for Testie, Expect
import "./pig-latin" for PigLatin

Testie.test("Pig Latin") {|do, skip|
  do.describe("ay is added to words that start with vowels") {
    do.test("word beginning with a") {
      Expect.value(PigLatin.translate("apple")).toEqual("appleay")
    }

    skip.test("word beginning with e") {
      Expect.value(PigLatin.translate("ear")).toEqual("earay")
    }

    skip.test("word beginning with i") {
      Expect.value(PigLatin.translate("igloo")).toEqual("iglooay")
    }

    skip.test("word beginning with o") {
      Expect.value(PigLatin.translate("object")).toEqual("objectay")
    }

    skip.test("word beginning with u") {
      Expect.value(PigLatin.translate("under")).toEqual("underay")
    }

    skip.test("word beginning with a vowel and followed by a qu") {
      Expect.value(PigLatin.translate("equal")).toEqual("equalay")
    }
  }

  do.describe("first letter and ay are moved to the end of words that start with consonants") {
    skip.test("word beginning with p") {
      Expect.value(PigLatin.translate("pig")).toEqual("igpay")
    }

    skip.test("word beginning with k") {
      Expect.value(PigLatin.translate("koala")).toEqual("oalakay")
    }

    skip.test("word beginning with x") {
      Expect.value(PigLatin.translate("xenon")).toEqual("enonxay")
    }

    skip.test("word beginning with q without a following u") {
      Expect.value(PigLatin.translate("qat")).toEqual("atqay")
    }
  }

  do.describe("some letter clusters are treated like a single consonant") {
    skip.test("word beginning with ch") {
      Expect.value(PigLatin.translate("chair")).toEqual("airchay")
    }

    skip.test("word beginning with qu") {
      Expect.value(PigLatin.translate("queen")).toEqual("eenquay")
    }

    skip.test("word beginning with qu and a preceding consonant") {
      Expect.value(PigLatin.translate("square")).toEqual("aresquay")
    }

    skip.test("word beginning with th") {
      Expect.value(PigLatin.translate("therapy")).toEqual("erapythay")
    }

    skip.test("word beginning with thr") {
      Expect.value(PigLatin.translate("thrush")).toEqual("ushthray")
    }

    skip.test("word beginning with sch") {
      Expect.value(PigLatin.translate("school")).toEqual("oolschay")
    }
  }

  do.describe("some letter clusters are treated like a single vowel") {
    skip.test("word beginning with yt") {
      Expect.value(PigLatin.translate("yttria")).toEqual("yttriaay")
    }

    skip.test("word beginning with xr") {
      Expect.value(PigLatin.translate("xray")).toEqual("xrayay")
    }
  }

  do.describe("position of y in a word determines if it is a consonant or a vowel") {
    skip.test("y is treated like a consonant at the beginning of a word") {
      Expect.value(PigLatin.translate("yellow")).toEqual("ellowyay")
    }

    skip.test("y is treated like a vowel at the end of a consonant cluster") {
      Expect.value(PigLatin.translate("rhythm")).toEqual("ythmrhay")
    }

    skip.test("y as second letter in two letter word") {
      Expect.value(PigLatin.translate("my")).toEqual("ymay")
    }
  }

  do.describe("phrases are translated") {
    skip.test("a whole phrase") {
      Expect.value(PigLatin.translate("quick fast run")).toEqual("ickquay astfay unray")
    }
  }
}
