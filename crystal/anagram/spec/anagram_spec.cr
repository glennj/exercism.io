require "spec"
require "../src/*"

describe "Anagram" do
  it "no matches" do
    Anagram.find("diaper", ["hello", "world", "zombies", "pants"]).should eq([] of String)
  end

  it "detects two anagrams" do
    Anagram.find("solemn", ["lemons", "cherry", "melons"]).should eq(["lemons", "melons"] of String)
  end

  it "does not detect anagram subsets" do
    Anagram.find("good", ["dog", "goody"]).should eq([] of String)
  end

  it "detects anagram" do
    Anagram.find("listen", ["enlists", "google", "inlets", "banana"]).should eq(["inlets"] of String)
  end

  it "detects three anagrams" do
    Anagram.find("allergy", ["gallery", "ballerina", "regally", "clergy", "largely", "leading"]).should eq(["gallery", "regally", "largely"] of String)
  end

  it "detects multiple anagrams with different case" do
    Anagram.find("nose", ["Eons", "ONES"]).should eq(["Eons", "ONES"] of String)
  end

  it "does not detect non-anagrams with identical checksum" do
    Anagram.find("mass", ["last"]).should eq([] of String)
  end

  it "detects anagrams case-insensitively" do
    Anagram.find("Orchestra", ["cashregister", "Carthorse", "radishes"]).should eq(["Carthorse"] of String)
  end

  it "detects anagrams using case-insensitive subject" do
    Anagram.find("Orchestra", ["cashregister", "carthorse", "radishes"]).should eq(["carthorse"] of String)
  end

  it "detects anagrams using case-insensitive possible matches" do
    Anagram.find("orchestra", ["cashregister", "Carthorse", "radishes"]).should eq(["Carthorse"] of String)
  end

  it "does not detect an anagram if the original word is repeated" do
    Anagram.find("go", ["go Go GO"]).should eq([] of String)
  end

  it "anagrams must use all letters exactly once" do
    Anagram.find("tapper", ["patter"]).should eq([] of String)
  end

  it "words are not anagrams of themselves" do
    Anagram.find("BANANA", ["BANANA"]).should eq([] of String)
  end

  it "words are not anagrams of themselves even if letter case is partially different" do
    Anagram.find("BANANA", ["Banana"]).should eq([] of String)
  end

  it "words are not anagrams of themselves even if letter case is completely different" do
    Anagram.find("BANANA", ["banana"]).should eq([] of String)
  end

  it "words other than themselves can be anagrams" do
    Anagram.find("LISTEN", ["LISTEN", "Silent"]).should eq(["Silent"] of String)
  end
end
