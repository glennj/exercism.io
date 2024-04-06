include std/unittest.e

include roman-numerals.ex

set_test_verbosity(TEST_SHOW_ALL)

procedure test_aquel(sequence name, object outcome, object expected) 
    test_equal(name,expected,outcome)
end procedure
test_aquel("1 is I",roman(1),"I")
test_aquel("2 is II",roman(2),"II")
test_aquel("3 is III",roman(3),"III")
test_aquel("4 is IV",roman(4),"IV")
test_aquel("5 is V",roman(5),"V")
test_aquel("6 is VI",roman(6),"VI")
test_aquel("9 is IX",roman(9),"IX")
test_aquel("16 is XVI",roman(16),"XVI")
test_aquel("27 is XXVII",roman(27),"XXVII")
test_aquel("48 is XLVIII",roman(48),"XLVIII")
test_aquel("49 is XLIX",roman(49),"XLIX")
test_aquel("59 is LIX",roman(59),"LIX")
test_aquel("66 is LXVI",roman(66),"LXVI")
test_aquel("93 is XCIII",roman(93),"XCIII")
test_aquel("141 is CXLI",roman(141),"CXLI")
test_aquel("163 is CLXIII",roman(163),"CLXIII")
test_aquel("166 is CLXVI",roman(166),"CLXVI")
test_aquel("402 is CDII",roman(402),"CDII")
test_aquel("575 is DLXXV",roman(575),"DLXXV")
test_aquel("666 is DCLXVI",roman(666),"DCLXVI")
test_aquel("911 is CMXI",roman(911),"CMXI")
test_aquel("1024 is MXXIV",roman(1024),"MXXIV")
test_aquel("1666 is MDCLXVI",roman(1666),"MDCLXVI")
test_aquel("3000 is MMM",roman(3000),"MMM")
test_aquel("3001 is MMMI",roman(3001),"MMMI")
test_aquel("3999 is MMMCMXCIX",roman(3999),"MMMCMXCIX")

test_report()
