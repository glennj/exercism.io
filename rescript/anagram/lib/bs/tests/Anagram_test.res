open Test
open Anagram

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("no matches", () => {equal(~message="no matches", findAnagrams("diaper", ["hello","world","zombies","pants"]), None)})

test("detects two anagrams", () => {equal(~message="detects two anagrams", findAnagrams("solemn", ["lemons","cherry","melons"]), Some(["lemons","melons"]))})

test("does not detect anagram subsets", () => {equal(~message="does not detect anagram subsets", findAnagrams("good", ["dog","goody"]), None)})

test("detects anagram", () => {equal(~message="detects anagram", findAnagrams("listen", ["enlists","google","inlets","banana"]), Some(["inlets"]))})

test("detects three anagrams", () => {equal(~message="detects three anagrams", findAnagrams("allergy", ["gallery","ballerina","regally","clergy","largely","leading"]), Some(["gallery","regally","largely"]))})

test("detects multiple anagrams with different case", () => {equal(~message="detects multiple anagrams with different case", findAnagrams("nose", ["Eons","ONES"]), Some(["Eons","ONES"]))})

test("does not detect non-anagrams with identical checksum", () => {equal(~message="does not detect non-anagrams with identical checksum", findAnagrams("mass", ["last"]), None)})

test("detects anagrams case-insensitively", () => {equal(~message="detects anagrams case-insensitively", findAnagrams("Orchestra", ["cashregister","Carthorse","radishes"]), Some(["Carthorse"]))})

test("detects anagrams using case-insensitive subject", () => {equal(~message="detects anagrams using case-insensitive subject", findAnagrams("Orchestra", ["cashregister","carthorse","radishes"]), Some(["carthorse"]))})

test("detects anagrams using case-insensitive possible matches", () => {equal(~message="detects anagrams using case-insensitive possible matches", findAnagrams("orchestra", ["cashregister","Carthorse","radishes"]), Some(["Carthorse"]))})

test("does not detect an anagram if the original word is repeated", () => {equal(~message="does not detect an anagram if the original word is repeated", findAnagrams("go", ["goGoGO"]), None)})

test("anagrams must use all letters exactly once", () => {equal(~message="anagrams must use all letters exactly once", findAnagrams("tapper", ["patter"]), None)})

test("words are not anagrams of themselves", () => {equal(~message="words are not anagrams of themselves", findAnagrams("BANANA", ["BANANA"]), None)})

test("words are not anagrams of themselves even if letter case is partially different", () => {equal(~message="words are not anagrams of themselves even if letter case is partially different", findAnagrams("BANANA", ["Banana"]), None)})

test("words are not anagrams of themselves even if letter case is completely different", () => {equal(~message="words are not anagrams of themselves even if letter case is completely different", findAnagrams("BANANA", ["banana"]), None)})

test("words other than themselves can be anagrams", () => {equal(~message="words other than themselves can be anagrams", findAnagrams("LISTEN", ["LISTEN","Silent"]), Some(["Silent"]))})

test("handles case of greek letters", () => {equal(~message="handles case of greek letters", findAnagrams("ΑΒΓ", ["ΒΓΑ","ΒΓΔ","γβα","αβγ"]), Some(["ΒΓΑ","γβα"]))})

test("different characters may have the same bytes", () => {equal(~message="different characters may have the same bytes", findAnagrams("a⬂", ["€a"]), None)})
