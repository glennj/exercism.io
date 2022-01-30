import "wren-testie/testie" for Testie, Expect
import "./allergies" for Allergies

Testie.test("Allergies") { |do, skip|
  do.describe("testing for eggs allergy") {
    do.test("not allergic to anything") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.allergicTo("eggs")).toEqual(false)
    }

    do.test("allergic only to eggs") {
      var allergies = Allergies.new(1)
      Expect.value(allergies.allergicTo("eggs")).toEqual(true)
    }

    do.test("allergic to eggs and something else") {
      var allergies = Allergies.new(3)
      Expect.value(allergies.allergicTo("eggs")).toEqual(true)
    }

    do.test("allergic to something, but not eggs") {
      var allergies = Allergies.new(2)
      Expect.value(allergies.allergicTo("eggs")).toEqual(false)
    }

    do.test("allergic to everything") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("eggs")).toEqual(true)
    }
  }

  do.describe("testing for peanuts allergy") {
    do.test("not allergic to anything") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.allergicTo("peanuts")).toEqual(false)
    }

    do.test("allergic only to peanuts") {
      var allergies = Allergies.new(2)
      Expect.value(allergies.allergicTo("peanuts")).toEqual(true)
    }

    do.test("allergic to peanuts and something else") {
      var allergies = Allergies.new(7)
      Expect.value(allergies.allergicTo("peanuts")).toEqual(true)
    }

    do.test("allergic to something, but not peanuts") {
      var allergies = Allergies.new(5)
      Expect.value(allergies.allergicTo("peanuts")).toEqual(false)
    }

    do.test("allergic to everything") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("peanuts")).toEqual(true)
    }
  }

  do.describe("testing for shellfish allergy") {
    do.test("not allergic to anything") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.allergicTo("shellfish")).toEqual(false)
    }

    do.test("allergic only to shellfish") {
      var allergies = Allergies.new(4)
      Expect.value(allergies.allergicTo("shellfish")).toEqual(true)
    }

    do.test("allergic to shellfish and something else") {
      var allergies = Allergies.new(14)
      Expect.value(allergies.allergicTo("shellfish")).toEqual(true)
    }

    do.test("allergic to something, but not shellfish") {
      var allergies = Allergies.new(10)
      Expect.value(allergies.allergicTo("shellfish")).toEqual(false)
    }

    do.test("allergic to everything") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("shellfish")).toEqual(true)
    }
  }

  do.describe("testing for strawberries allergy") {
    do.test("not allergic to anything") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.allergicTo("strawberries")).toEqual(false)
    }

    do.test("allergic only to strawberries") {
      var allergies = Allergies.new(8)
      Expect.value(allergies.allergicTo("strawberries")).toEqual(true)
    }

    do.test("allergic to strawberries and something else") {
      var allergies = Allergies.new(28)
      Expect.value(allergies.allergicTo("strawberries")).toEqual(true)
    }

    do.test("allergic to something, but not strawberries") {
      var allergies = Allergies.new(20)
      Expect.value(allergies.allergicTo("strawberries")).toEqual(false)
    }

    do.test("allergic to everything") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("strawberries")).toEqual(true)
    }
  }

  do.describe("testing for tomatoes allergy") {
    do.test("not allergic to anything") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.allergicTo("tomatoes")).toEqual(false)
    }

    do.test("allergic only to tomatoes") {
      var allergies = Allergies.new(16)
      Expect.value(allergies.allergicTo("tomatoes")).toEqual(true)
    }

    do.test("allergic to tomatoes and something else") {
      var allergies = Allergies.new(56)
      Expect.value(allergies.allergicTo("tomatoes")).toEqual(true)
    }

    do.test("allergic to something, but not tomatoes") {
      var allergies = Allergies.new(40)
      Expect.value(allergies.allergicTo("tomatoes")).toEqual(false)
    }

    do.test("allergic to everything") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("tomatoes")).toEqual(true)
    }
  }

  do.describe("testing for chocolate allergy") {
    do.test("not allergic to anything") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.allergicTo("chocolate")).toEqual(false)
    }

    do.test("allergic only to chocolate") {
      var allergies = Allergies.new(32)
      Expect.value(allergies.allergicTo("chocolate")).toEqual(true)
    }

    do.test("allergic to chocolate and something else") {
      var allergies = Allergies.new(112)
      Expect.value(allergies.allergicTo("chocolate")).toEqual(true)
    }

    do.test("allergic to something, but not chocolate") {
      var allergies = Allergies.new(80)
      Expect.value(allergies.allergicTo("chocolate")).toEqual(false)
    }

    do.test("allergic to everything") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("chocolate")).toEqual(true)
    }
  }

  do.describe("testing for pollen allergy") {
    do.test("not allergic to anything") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.allergicTo("pollen")).toEqual(false)
    }

    do.test("allergic only to pollen") {
      var allergies = Allergies.new(64)
      Expect.value(allergies.allergicTo("pollen")).toEqual(true)
    }

    do.test("allergic to pollen and something else") {
      var allergies = Allergies.new(224)
      Expect.value(allergies.allergicTo("pollen")).toEqual(true)
    }

    do.test("allergic to something, but not pollen") {
      var allergies = Allergies.new(160)
      Expect.value(allergies.allergicTo("pollen")).toEqual(false)
    }

    do.test("allergic to everything") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("pollen")).toEqual(true)
    }
  }

  do.describe("testing for cats allergy") {
    do.test("not allergic to anything") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.allergicTo("cats")).toEqual(false)
    }

    do.test("allergic only to cats") {
      var allergies = Allergies.new(128)
      Expect.value(allergies.allergicTo("cats")).toEqual(true)
    }

    do.test("allergic to cats and something else") {
      var allergies = Allergies.new(192)
      Expect.value(allergies.allergicTo("cats")).toEqual(true)
    }

    do.test("allergic to something, but not cats") {
      var allergies = Allergies.new(64)
      Expect.value(allergies.allergicTo("cats")).toEqual(false)
    }

    do.test("allergic to everything") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("cats")).toEqual(true)
    }
  }

  do.describe("list when:") {
    do.test("no allergies") {
      var allergies = Allergies.new(0)
      Expect.value(allergies.list()).toEqual([])
    }

    do.test("just eggs") {
      var allergies = Allergies.new(1)
      Expect.value(allergies.list()).toEqual(["eggs"])
    }

    do.test("just peanuts") {
      var allergies = Allergies.new(2)
      Expect.value(allergies.list()).toEqual(["peanuts"])
    }

    do.test("just strawberries") {
      var allergies = Allergies.new(8)
      Expect.value(allergies.list()).toEqual(["strawberries"])
    }

    do.test("eggs and peanuts") {
      var allergies = Allergies.new(3)
      Expect.value(allergies.list()).toIncludeSameItemsAs(["eggs", "peanuts"])
    }

    do.test("more than eggs but not peanuts") {
      var allergies = Allergies.new(5)
      Expect.value(allergies.list()).toIncludeSameItemsAs(["eggs", "shellfish"])
    }

    do.test("lots of stuff") {
      var allergies = Allergies.new(248)
      Expect.value(allergies.list()).toIncludeSameItemsAs([
        "strawberries",
        "tomatoes",
        "chocolate",
        "pollen",
        "cats",
      ])
    }

    do.test("everything") {
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

    do.test("no allergen score parts") {
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
    do.test("not allergic to random things") {
      var allergies = Allergies.new(255)
      Expect.value(allergies.allergicTo("pankcakes")).toEqual(false)
      Expect.value(allergies.allergicTo("mushrooms")).toEqual(false)
      Expect.value(allergies.allergicTo("water")).toEqual(false)
      Expect.value(allergies.allergicTo("milk")).toEqual(false)
      Expect.value(allergies.allergicTo("candy")).toEqual(false)
    }
  }
}
