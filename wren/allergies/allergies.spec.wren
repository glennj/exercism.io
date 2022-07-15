import "wren-testie/testie" for Testie, Expect
import "./allergies" for Allergies

Testie.test("Allergies") { |do, skip|
  do.describe("testing for eggs allergy") {
    do.test("not allergic to anything") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.allergicTo("eggs")).toEqual(false)
    }

    skip.test("allergic only to eggs") {
      var allergies = Allergies.new(1)
      Expect.value(allergies.allergicTo("eggs")).toEqual(true)
    }

    skip.test("allergic to eggs and something else") {
      var allergies = Allergies.new(3)
      Expect.value(allergies.allergicTo("eggs")).toEqual(true)
    }

    skip.test("allergic to something, but not eggs") {
      var allergies = Allergies.new(2)
      Expect.value(allergies.allergicTo("eggs")).toEqual(false)
    }

    skip.test("allergic to everything") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("eggs")).toEqual(true)
    }
  }

  do.describe("testing for peanuts allergy") {
    skip.test("not allergic to anything") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.allergicTo("peanuts")).toEqual(false)
    }

    skip.test("allergic only to peanuts") {
      var allergies = Allergies.new(2)
      Expect.value(allergies.allergicTo("peanuts")).toEqual(true)
    }

    skip.test("allergic to peanuts and something else") {
      var allergies = Allergies.new(7)
      Expect.value(allergies.allergicTo("peanuts")).toEqual(true)
    }

    skip.test("allergic to something, but not peanuts") {
      var allergies = Allergies.new(5)
      Expect.value(allergies.allergicTo("peanuts")).toEqual(false)
    }

    skip.test("allergic to everything") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("peanuts")).toEqual(true)
    }
  }

  do.describe("testing for shellfish allergy") {
    skip.test("not allergic to anything") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.allergicTo("shellfish")).toEqual(false)
    }

    skip.test("allergic only to shellfish") {
      var allergies = Allergies.new(4)
      Expect.value(allergies.allergicTo("shellfish")).toEqual(true)
    }

    skip.test("allergic to shellfish and something else") {
      var allergies = Allergies.new(14)
      Expect.value(allergies.allergicTo("shellfish")).toEqual(true)
    }

    skip.test("allergic to something, but not shellfish") {
      var allergies = Allergies.new(10)
      Expect.value(allergies.allergicTo("shellfish")).toEqual(false)
    }

    skip.test("allergic to everything") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("shellfish")).toEqual(true)
    }
  }

  do.describe("testing for strawberries allergy") {
    skip.test("not allergic to anything") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.allergicTo("strawberries")).toEqual(false)
    }

    skip.test("allergic only to strawberries") {
      var allergies = Allergies.new(8)
      Expect.value(allergies.allergicTo("strawberries")).toEqual(true)
    }

    skip.test("allergic to strawberries and something else") {
      var allergies = Allergies.new(28)
      Expect.value(allergies.allergicTo("strawberries")).toEqual(true)
    }

    skip.test("allergic to something, but not strawberries") {
      var allergies = Allergies.new(20)
      Expect.value(allergies.allergicTo("strawberries")).toEqual(false)
    }

    skip.test("allergic to everything") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("strawberries")).toEqual(true)
    }
  }

  do.describe("testing for tomatoes allergy") {
    skip.test("not allergic to anything") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.allergicTo("tomatoes")).toEqual(false)
    }

    skip.test("allergic only to tomatoes") {
      var allergies = Allergies.new(16)
      Expect.value(allergies.allergicTo("tomatoes")).toEqual(true)
    }

    skip.test("allergic to tomatoes and something else") {
      var allergies = Allergies.new(56)
      Expect.value(allergies.allergicTo("tomatoes")).toEqual(true)
    }

    skip.test("allergic to something, but not tomatoes") {
      var allergies = Allergies.new(40)
      Expect.value(allergies.allergicTo("tomatoes")).toEqual(false)
    }

    skip.test("allergic to everything") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("tomatoes")).toEqual(true)
    }
  }

  do.describe("testing for chocolate allergy") {
    skip.test("not allergic to anything") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.allergicTo("chocolate")).toEqual(false)
    }

    skip.test("allergic only to chocolate") {
      var allergies = Allergies.new(32)
      Expect.value(allergies.allergicTo("chocolate")).toEqual(true)
    }

    skip.test("allergic to chocolate and something else") {
      var allergies = Allergies.new(112)
      Expect.value(allergies.allergicTo("chocolate")).toEqual(true)
    }

    skip.test("allergic to something, but not chocolate") {
      var allergies = Allergies.new(80)
      Expect.value(allergies.allergicTo("chocolate")).toEqual(false)
    }

    skip.test("allergic to everything") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("chocolate")).toEqual(true)
    }
  }

  do.describe("testing for pollen allergy") {
    skip.test("not allergic to anything") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.allergicTo("pollen")).toEqual(false)
    }

    skip.test("allergic only to pollen") {
      var allergies = Allergies.new(64)
      Expect.value(allergies.allergicTo("pollen")).toEqual(true)
    }

    skip.test("allergic to pollen and something else") {
      var allergies = Allergies.new(224)
      Expect.value(allergies.allergicTo("pollen")).toEqual(true)
    }

    skip.test("allergic to something, but not pollen") {
      var allergies = Allergies.new(160)
      Expect.value(allergies.allergicTo("pollen")).toEqual(false)
    }

    skip.test("allergic to everything") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("pollen")).toEqual(true)
    }
  }

  do.describe("testing for cats allergy") {
    skip.test("not allergic to anything") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.allergicTo("cats")).toEqual(false)
    }

    skip.test("allergic only to cats") {
      var allergies = Allergies.new(128)
      Expect.value(allergies.allergicTo("cats")).toEqual(true)
    }

    skip.test("allergic to cats and something else") {
      var allergies = Allergies.new(192)
      Expect.value(allergies.allergicTo("cats")).toEqual(true)
    }

    skip.test("allergic to something, but not cats") {
      var allergies = Allergies.new(64)
      Expect.value(allergies.allergicTo("cats")).toEqual(false)
    }

    skip.test("allergic to everything") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("cats")).toEqual(true)
    }
  }

  do.describe("list when:") {
    skip.test("no allergies") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.list()).toEqual([])
    }

    skip.test("just eggs") {
      var allergies = Allergies.new(1)
      Expect.value(allergies.list()).toEqual(["eggs"])
    }

    skip.test("just peanuts") {
      var allergies = Allergies.new(2)
      Expect.value(allergies.list()).toEqual(["peanuts"])
    }

    skip.test("just strawberries") {
      var allergies = Allergies.new(8)
      Expect.value(allergies.list()).toEqual(["strawberries"])
    }

    skip.test("eggs and peanuts") {
      var allergies = Allergies.new(3)
      Expect.value(allergies.list()).toIncludeSameItemsAs(["eggs", "peanuts"])
    }

    skip.test("more than eggs but not peanuts") {
      var allergies = Allergies.new(5)
      Expect.value(allergies.list()).toIncludeSameItemsAs(["eggs", "shellfish"])
    }

    skip.test("lots of stuff") {
      var allergies = Allergies.new(248)
      Expect.value(allergies.list()).toIncludeSameItemsAs([
        "strawberries",
        "tomatoes",
        "chocolate",
        "pollen",
        "cats",
      ])
    }

    skip.test("everything") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.list()).toIncludeSameItemsAs([
        "eggs",
        "peanuts",
        "shellfish",
        "strawberries",
        "tomatoes",
        "chocolate",
        "pollen",
        "cats",
      ])
    }

    skip.test("no allergen score parts") {
      var allergies = Allergies.new(509)
      Expect.value(allergies.list()).toIncludeSameItemsAs([
        "eggs",
        "shellfish",
        "strawberries",
        "tomatoes",
        "chocolate",
        "pollen",
        "cats",
      ])
    }
  }

  do.describe("random allergies") {
    skip.test("not allergic to random things") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("pankcakes")).toEqual(false)
      Expect.value(allergies.allergicTo("mushrooms")).toEqual(false)
      Expect.value(allergies.allergicTo("water")).toEqual(false)
      Expect.value(allergies.allergicTo("milk")).toEqual(false)
      Expect.value(allergies.allergicTo("candy")).toEqual(false)
    }
  }
}
