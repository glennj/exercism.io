import "./kindergarten-garden" for KindergartenGarden
import "wren-testie/testie" for Testie, Expect

Testie.test("kindergarten-garden.steps()") { |do, skip|
  do.test("partial garden -> garden with single student") {
    Expect.value(KindergartenGarden.plants("RC\nGG", "Alice")).toEqual(["radishes", "clover", "grass", "grass"])
  }

  do.test("partial garden -> different garden with single student") {
    Expect.value(KindergartenGarden.plants("VC\nRC", "Alice")).toEqual(["violets", "clover", "radishes", "clover"])
  }

  do.test("partial garden -> garden with two students") {
    Expect.value(KindergartenGarden.plants("VVCG\nVVRC", "Bob")).toEqual(["clover", "grass", "radishes", "clover"])
  }

  do.test("partial garden -> multiple students for the same garden with three students -> second student's garden") {
    Expect.value(KindergartenGarden.plants("VVCCGG\nVVCCGG", "Bob")).toEqual(["clover", "clover", "clover", "clover"])
  }

  do.test("partial garden -> multiple students for the same garden with three students -> third student's garden") {
    Expect.value(KindergartenGarden.plants("VVCCGG\nVVCCGG", "Charlie")).toEqual(["grass", "grass", "grass", "grass"])
  }

  do.test("full garden -> for Alice, first student's garden") {
    Expect.value(KindergartenGarden.plants("VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV", "Alice")).toEqual(["violets", "radishes", "violets", "radishes"])
  }

  do.test("full garden -> for Bob, second student's garden") {
    Expect.value(KindergartenGarden.plants("VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV", "Bob")).toEqual(["clover", "grass", "clover", "clover"])
  }

  do.test("full garden -> for Charlie") {
    Expect.value(KindergartenGarden.plants("VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV", "Charlie")).toEqual(["violets", "violets", "clover", "grass"])
  }

  do.test("full garden -> for David") {
    Expect.value(KindergartenGarden.plants("VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV", "David")).toEqual(["radishes", "violets", "clover", "radishes"])
  }

  do.test("full garden -> for Eve") {
    Expect.value(KindergartenGarden.plants("VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV", "Eve")).toEqual(["clover", "grass", "radishes", "grass"])
  }

  do.test("full garden -> for Fred") {
    Expect.value(KindergartenGarden.plants("VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV", "Fred")).toEqual(["grass", "clover", "violets", "clover"])
  }

  do.test("full garden -> for Ginny") {
    Expect.value(KindergartenGarden.plants("VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV", "Ginny")).toEqual(["clover", "grass", "grass", "clover"])
  }

  do.test("full garden -> for Harriet") {
    Expect.value(KindergartenGarden.plants("VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV", "Harriet")).toEqual(["violets", "radishes", "radishes", "violets"])
  }

  do.test("full garden -> for Ileana") {
    Expect.value(KindergartenGarden.plants("VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV", "Ileana")).toEqual(["grass", "clover", "violets", "clover"])
  }

  do.test("full garden -> for Joseph") {
    Expect.value(KindergartenGarden.plants("VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV", "Joseph")).toEqual(["violets", "clover", "violets", "grass"])
  }

  do.test("full garden -> for Kincaid, second to last student's garden") {
    Expect.value(KindergartenGarden.plants("VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV", "Kincaid")).toEqual(["grass", "clover", "clover", "grass"])
  }

  do.test("full garden -> for Larry, last student's garden") {
    Expect.value(KindergartenGarden.plants("VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV", "Larry")).toEqual(["grass", "violets", "clover", "violets"])
  }
}
