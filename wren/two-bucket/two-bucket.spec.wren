import "wren-testie/testie" for Testie, Expect
import "./two-bucket" for TwoBucket, TwoBucketError

Testie.test("Two Bucket") { |do, skip|
  do.test("Measure using bucket one of size 3 and bucket two of size 5 - start with bucket one") {
    var result = TwoBucket.measure({"bucketOne": 3, "bucketTwo": 5, "goal": 1, "startBucket": "one"})
    Expect.value(result).toEqual({"moves": 4, "goalBucket": "one", "otherBucket": 5})
  }

  do.test("Measure using bucket one of size 3 and bucket two of size 5 - start with bucket two") {
    var result = TwoBucket.measure({"bucketOne": 3, "bucketTwo": 5, "goal": 1, "startBucket": "two"})
    Expect.value(result).toEqual({"moves": 8, "goalBucket": "two", "otherBucket": 3})
  }

  do.test("Measure using bucket one of size 7 and bucket two of size 11 - start with bucket one") {
    var result = TwoBucket.measure({"bucketOne": 7, "bucketTwo": 11, "goal": 2, "startBucket": "one"})
    Expect.value(result).toEqual({"moves": 14, "goalBucket": "one", "otherBucket": 11})
  }

  do.test("Measure using bucket one of size 7 and bucket two of size 11 - start with bucket two") {
    var result = TwoBucket.measure({"bucketOne": 7, "bucketTwo": 11, "goal": 2, "startBucket": "two"})
    Expect.value(result).toEqual({"moves": 18, "goalBucket": "two", "otherBucket": 7})
  }

  do.test("Measure one step using bucket one of size 2 and bucket two of size 3 - start with bucket two") {
    var result = TwoBucket.measure({"bucketOne": 2, "bucketTwo": 3, "goal": 3, "startBucket": "two"})
    Expect.value(result).toEqual({"moves": 1, "goalBucket": "two", "otherBucket": 0})
  }

  do.test("Measure using bucket one of size 2 and bucket two of size 3 - start with bucket one and end with bucket two") {
    var result = TwoBucket.measure({"bucketOne": 2, "bucketTwo": 3, "goal": 3, "startBucket": "one"})
    Expect.value(result).toEqual({"moves": 2, "goalBucket": "two", "otherBucket": 2})
  }

  do.test("Not possible to reach the goal") {
    Expect.that {
      TwoBucket.measure({"bucketOne": 6, "bucketTwo": 15, "goal": 5, "startBucket": "one"})
    }.abortsWith(TwoBucketError)
  }

  do.test("With the same buckets but a different goal, then it is possible") {
    var result = TwoBucket.measure({"bucketOne": 6, "bucketTwo": 15, "goal": 9, "startBucket": "one"})
    Expect.value(result).toEqual({"moves": 10, "goalBucket": "two", "otherBucket": 0})
  }

  do.test("Goal larger than both buckets is impossible") {
    Expect.that {
      TwoBucket.measure({"bucketOne": 5, "bucketTwo": 7, "goal": 8, "startBucket": "one"})
    }.abortsWith(TwoBucketError)
  }
}
