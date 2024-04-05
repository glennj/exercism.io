include std/unittest.e

include roman-numerals.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("1 is I",roman(1),"I")
test_equal("2 is II",roman(2),"II")
test_equal("3 is III",roman(3),"III")
test_equal("4 is IV",roman(4),"IV")
test_equal("5 is V",roman(5),"V")
test_equal("6 is VI",roman(6),"VI")
test_equal("9 is IX",roman(9),"IX")
test_equal("16 is XVI",roman(16),"XVI")
test_equal("27 is XXVII",roman(27),"XXVII")
test_equal("48 is XLVIII",roman(48),"XLVIII")
test_equal("49 is XLIX",roman(49),"XLIX")
test_equal("59 is LIX",roman(59),"LIX")
test_equal("66 is LXVI",roman(66),"LXVI")
test_equal("93 is XCIII",roman(93),"XCIII")
test_equal("141 is CXLI",roman(141),"CXLI")
test_equal("163 is CLXIII",roman(163),"CLXIII")
test_equal("166 is CLXVI",roman(166),"CLXVI")
test_equal("402 is CDII",roman(402),"CDII")
test_equal("575 is DLXXV",roman(575),"DLXXV")
test_equal("666 is DCLXVI",roman(666),"DCLXVI")
test_equal("911 is CMXI",roman(911),"CMXI")
test_equal("1024 is MXXIV",roman(1024),"MXXIV")
test_equal("1666 is MDCLXVI",roman(1666),"MDCLXVI")
test_equal("3000 is MMM",roman(3000),"MMM")
test_equal("3001 is MMMI",roman(3001),"MMMI")
test_equal("3999 is MMMCMXCIX",roman(3999),"MMMCMXCIX")

test_report()
