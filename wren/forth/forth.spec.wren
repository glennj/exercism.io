import "wren-testie/testie" for Testie, Expect
import "./forth" for Forth

var FORTH = null

Testie.test("Forth") { |do, skip|
  do.beforeEach {
    FORTH = Forth.new()
  }

  do.describe("parsing and numbers") {
    do.test("numbers just get pushed onto the stack") {
      FORTH.evaluate("1 2 3 4 5")
      Expect.value(FORTH.stack).toEqual([1, 2, 3, 4, 5])
    }

    skip.test("pushes negative numbers onto the stack") {
      FORTH.evaluate("-1 -2 -3 -4 -5")
      Expect.value(FORTH.stack).toEqual([-1, -2, -3, -4, -5])
    }
  }

  do.describe("addition") {
    skip.test("can add two numbers") {
      FORTH.evaluate("1 2 +")
      Expect.value(FORTH.stack).toEqual([3])
    }

    skip.test("errors if there is nothing on the stack") {
      Expect.that {
        FORTH.evaluate("+")
      }.abortsWith("Stack empty")
    }

    skip.test("errors if there is only one value on the stack") {
      Expect.that {
        FORTH.evaluate("1 +")
      }.abortsWith("Stack empty")
    }
  }

  do.describe("subtraction") {
    skip.test("can subtract two numbers") {
      FORTH.evaluate("3 4 -")
      Expect.value(FORTH.stack).toEqual([-1])
    }

    skip.test("errors if there is nothing on the stack") {
      Expect.that {
        FORTH.evaluate("-")
      }.abortsWith("Stack empty")
    }

    skip.test("errors if there is only one value on the stack") {
      Expect.that {
        FORTH.evaluate("1 -")
      }.abortsWith("Stack empty")
    }
  }

  do.describe("multiplication") {
    skip.test("can multiply two numbers") {
      FORTH.evaluate("2 4 *")
      Expect.value(FORTH.stack).toEqual([8])
    }

    skip.test("errors if there is nothing on the stack") {
      Expect.that {
        FORTH.evaluate("*")
      }.abortsWith("Stack empty")
    }

    skip.test("errors if there is only one value on the stack") {
      Expect.that {
        FORTH.evaluate("1 *")
      }.abortsWith("Stack empty")
    }
  }

  do.describe("division") {
    skip.test("can divide two numbers") {
      FORTH.evaluate("12 3 /")
      Expect.value(FORTH.stack).toEqual([4])
    }

    skip.test("performs integer division") {
      FORTH.evaluate("8 3 /")
      Expect.value(FORTH.stack).toEqual([2])
    }

    skip.test("errors if dividing by zero") {
      Expect.that {
        FORTH.evaluate("4 0 /")
      }.abortsWith("Division by zero")
    }

    skip.test("errors if there is nothing on the stack") {
      Expect.that {
        FORTH.evaluate("/")
      }.abortsWith("Stack empty")
    }

    skip.test("errors if there is only one value on the stack") {
      Expect.that {
        FORTH.evaluate("1 /")
      }.abortsWith("Stack empty")
    }
  }

  do.describe("combined arithmetic") {
    skip.test("addition and subtraction") {
      FORTH.evaluate("1 2 + 4 -")
      Expect.value(FORTH.stack).toEqual([-1])
    }

    skip.test("multiplication and division") {
      FORTH.evaluate("2 4 * 3 /")
      Expect.value(FORTH.stack).toEqual([2])
    }
  }

  do.describe("dup") {
    skip.test("copies a value on the stack") {
      FORTH.evaluate("1 dup")
      Expect.value(FORTH.stack).toEqual([1, 1])
    }

    skip.test("copies the top value on the stack") {
      FORTH.evaluate("1 2 dup")
      Expect.value(FORTH.stack).toEqual([1, 2, 2])
    }

    skip.test("errors if there is nothing on the stack") {
      Expect.that {
        FORTH.evaluate("dup")
      }.abortsWith("Stack empty")
    }
  }

  do.describe("drop") {
    skip.test("removes the top value on the stack if it is the only one") {
      FORTH.evaluate("1 drop")
      Expect.value(FORTH.stack).toEqual([])
    }

    skip.test("removes the top value on the stack if it is not the only one") {
      FORTH.evaluate("1 2 drop")
      Expect.value(FORTH.stack).toEqual([1])
    }

    skip.test("errors if there is nothing on the stack") {
      Expect.that {
        FORTH.evaluate("drop")
      }.abortsWith("Stack empty")
    }
  }

  do.describe("swap") {
    skip.test("swaps the top two values on the stack if they are the only ones") {
      FORTH.evaluate("1 2 swap")
      Expect.value(FORTH.stack).toEqual([2, 1])
    }

    skip.test("swaps the top two values on the stack if they are not the only ones") {
      FORTH.evaluate("1 2 3 swap")
      Expect.value(FORTH.stack).toEqual([1, 3, 2])
    }

    skip.test("errors if there is nothing on the stack") {
      Expect.that {
        FORTH.evaluate("swap")
      }.abortsWith("Stack empty")
    }

    skip.test("errors if there is only one value on the stack") {
      Expect.that {
        FORTH.evaluate("1 swap")
      }.abortsWith("Stack empty")
    }
  }

  do.describe("over") {
    skip.test("copies the second element if there are only two") {
      FORTH.evaluate("1 2 over")
      Expect.value(FORTH.stack).toEqual([1, 2, 1])
    }

    skip.test("copies the second element if there are more than two") {
      FORTH.evaluate("1 2 3 over")
      Expect.value(FORTH.stack).toEqual([1, 2, 3, 2])
    }

    skip.test("errors if there is nothing on the stack") {
      Expect.that {
        FORTH.evaluate("over")
      }.abortsWith("Stack empty")
    }

    skip.test("errors if there is only one value on the stack") {
      Expect.that {
        FORTH.evaluate("1 over")
      }.abortsWith("Stack empty")
    }
  }

  do.describe("user-defined words") {
    skip.test("can consist of built-in words") {
      FORTH.evaluate(": dup-twice dup dup ;")
      FORTH.evaluate("1 dup-twice")
      Expect.value(FORTH.stack).toEqual([1, 1, 1])
    }

    skip.test("execute in the right order") {
      FORTH.evaluate(": countup 1 2 3 ;")
      FORTH.evaluate("countup")
      Expect.value(FORTH.stack).toEqual([1, 2, 3])
    }

    skip.test("can override other user-defined words") {
      FORTH.evaluate(": foo dup ;")
      FORTH.evaluate(": foo dup dup ;")
      FORTH.evaluate("1 foo")
      Expect.value(FORTH.stack).toEqual([1, 1, 1])
    }

    skip.test("can override built-in words") {
      FORTH.evaluate(": swap dup ;")
      FORTH.evaluate("1 swap")
      Expect.value(FORTH.stack).toEqual([1, 1])
    }

    skip.test("can override built-in operators") {
      FORTH.evaluate(": + * ;")
      FORTH.evaluate("3 4 +")
      Expect.value(FORTH.stack).toEqual([12])
    }

    skip.test("can use different words with the same name") {
      FORTH.evaluate(": foo 5 ;")
      FORTH.evaluate(": bar foo ;")
      FORTH.evaluate(": foo 6 ;")
      FORTH.evaluate("bar foo")
      Expect.value(FORTH.stack).toEqual([5, 6])
    }

    skip.test("can define word that uses word with the same name") {
      FORTH.evaluate(": foo 10 ;")
      FORTH.evaluate(": foo foo 1 + ;")
      FORTH.evaluate("foo")
      Expect.value(FORTH.stack).toEqual([11])
    }

    skip.test("cannot redefine numbers") {
      Expect.that {
        FORTH.evaluate(": 1 2 ;")
      }.abortsWith("Invalid definition")
    }
    skip.test("cannot redefine negative numbers") {
      Expect.that {
        FORTH.evaluate(": -1 2 ;")
      }.abortsWith("Invalid definition")
    }

    skip.test("errors if executing a non-existent word") {
      Expect.that {
        FORTH.evaluate("foo")
      }.abortsWith("Unknown command")
    }

    skip.test("only defines locally") {
      var first = Forth.new()
      var second = Forth.new()
      first.evaluate(": + - ;")
      first.evaluate("1 1 +")
      second.evaluate("1 1 +")
      Expect.value(first.stack).toEqual([0])
      Expect.value(second.stack).toEqual([2])
    }
  }

  do.describe("case-insensitivity") {
    skip.test("DUP is case-insensitive") {
      FORTH.evaluate("1 DUP Dup dup")
      Expect.value(FORTH.stack).toEqual([1, 1, 1, 1])
    }

    skip.test("DROP is case-insensitive") {
      FORTH.evaluate("1 2 3 4 DROP Drop drop")
      Expect.value(FORTH.stack).toEqual([1])
    }

    skip.test("SWAP is case-insensitive") {
      FORTH.evaluate("1 2 SWAP 3 Swap 4 swap")
      Expect.value(FORTH.stack).toEqual([2, 3, 4, 1])
    }

    skip.test("OVER is case-insensitive") {
      FORTH.evaluate("1 2 OVER Over over")
      Expect.value(FORTH.stack).toEqual([1, 2, 1, 2, 1])
    }

    skip.test("user-defined words are case-insensitive") {
      FORTH.evaluate(": foo dup ;")
      FORTH.evaluate("1 FOO Foo foo")
      Expect.value(FORTH.stack).toEqual([1, 1, 1, 1])
    }

    skip.test("definitions are case-insensitive") {
      FORTH.evaluate(": SWAP DUP Dup dup ;")
      FORTH.evaluate("1 swap")
      Expect.value(FORTH.stack).toEqual([1, 1, 1, 1])
    }
  }
}
