import "./micro-blog" for MicroBlog
import "wren-testie/testie" for Testie, Expect

Testie.test("MicroBlog") { |do, skip|
  do.test("English language short") {
    Expect.value(MicroBlog.truncate("Hi")).toEqual("Hi")
  }

  do.test("English language long") {
    Expect.value(MicroBlog.truncate("Hello there")).toEqual("Hello")
  }

  do.test("German language short (broth)") {
    Expect.value(MicroBlog.truncate("brühe")).toEqual("brühe")
  }

  do.test("German language long (bear carpet → beards)") {
    Expect.value(MicroBlog.truncate("Bärteppich")).toEqual("Bärte")
  }

  do.test("Bulgarian language short (good)") {
    Expect.value(MicroBlog.truncate("Добър")).toEqual("Добър")
  }

  do.test("Greek language short (health)") {
    Expect.value(MicroBlog.truncate("υγειά")).toEqual("υγειά")
  }

  do.test("Maths short") {
    Expect.value(MicroBlog.truncate("a=πr²")).toEqual("a=πr²")
  }

  do.test("Maths long") {
    Expect.value(MicroBlog.truncate("∅⊊ℕ⊊ℤ⊊ℚ⊊ℝ⊊ℂ")).toEqual("∅⊊ℕ⊊ℤ")
  }

  do.test("English and emoji short") {
    Expect.value(MicroBlog.truncate("Fly 🛫")).toEqual("Fly 🛫")
  }

  do.test("Emoji short") {
    Expect.value(MicroBlog.truncate("💇")).toEqual("💇")
  }

  do.test("Emoji long") {
    Expect.value(MicroBlog.truncate("❄🌡🤧🤒🏥🕰😀")).toEqual("❄🌡🤧🤒🏥")
  }

  do.test("Royal Flush?") {
    Expect.value(MicroBlog.truncate("🃎🂸🃅🃋🃍🃁🃊")).toEqual("🃎🂸🃅🃋🃍")
  }
}
