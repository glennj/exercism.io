KindergartenGarden = require "./kindergarten-garden"

describe "Kindergarten Garden", ->
  describe "partial garden", ->
    it "garden with single student", ->
      diagram = new KindergartenGarden "RC\nGG"
      expect(diagram.plants "Alice").toEqual ["radishes", "clover", "grass", "grass"]

    it "different garden with single student", ->
        diagram = new KindergartenGarden "VC\nRC"
        expect(diagram.plants "Alice").toEqual ["violets", "clover", "radishes", "clover"]

    it "garden with two students", ->
        diagram = new KindergartenGarden "VVCG\nVVRC"
        expect(diagram.plants "Bob").toEqual ["clover", "grass", "radishes", "clover"]

  describe "multiple students for the same garden with three students", ->
    it "second student's garden", ->
        diagram = new KindergartenGarden "VVCCGG\nVVCCGG"
        expect(diagram.plants "Bob").toEqual ["clover", "clover", "clover", "clover"]

    it "third student's garden", ->
        diagram = new KindergartenGarden "VVCCGG\nVVCCGG"
        expect(diagram.plants "Charlie").toEqual ["grass", "grass", "grass", "grass"]

  describe "full garden", ->
    it "for Alice, first student's garden", ->
        diagram = new KindergartenGarden "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
        expect(diagram.plants "Alice").toEqual ["violets", "radishes", "violets", "radishes"]

    it "for Bob, second student's garden", ->
        diagram = new KindergartenGarden "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
        expect(diagram.plants "Bob").toEqual ["clover", "grass", "clover", "clover"]

    it "for Charlie", ->
        diagram = new KindergartenGarden "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
        expect(diagram.plants "Charlie").toEqual ["violets", "violets", "clover", "grass"]

    it "for David", ->
        diagram = new KindergartenGarden "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
        expect(diagram.plants "David").toEqual ["radishes", "violets", "clover", "radishes"]

    it "for Eve", ->
        diagram = new KindergartenGarden "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
        expect(diagram.plants "Eve").toEqual ["clover", "grass", "radishes", "grass"]

    it "for Fred", ->
        diagram = new KindergartenGarden "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
        expect(diagram.plants "Fred").toEqual ["grass", "clover", "violets", "clover"]

    it "for Ginny", ->
        diagram = new KindergartenGarden "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
        expect(diagram.plants "Ginny").toEqual ["clover", "grass", "grass", "clover"]

    it "for Harriet", ->
        diagram = new KindergartenGarden "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
        expect(diagram.plants "Harriet").toEqual ["violets", "radishes", "radishes", "violets"]

    it "for Ileana", ->
        diagram = new KindergartenGarden "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
        expect(diagram.plants "Ileana").toEqual ["grass", "clover", "violets", "clover"]

    it "for Joseph", ->
        diagram = new KindergartenGarden "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
        expect(diagram.plants "Joseph").toEqual ["violets", "clover", "violets", "grass"]

    it "for Kincaid, second to last student's garden", ->
        diagram = new KindergartenGarden "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
        expect(diagram.plants "Kincaid").toEqual ["grass", "clover", "clover", "grass"]

    it "for Larry, last student's garden", ->
        diagram = new KindergartenGarden "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
        expect(diagram.plants "Larry").toEqual ["grass", "violets", "clover", "violets"]
