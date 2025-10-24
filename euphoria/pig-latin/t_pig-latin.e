include std/unittest.e

include pig-latin.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("ay is added to words that start with vowels - word beginning with a", 
           "appleay",
           translate("apple"))
test_equal("ay is added to words that start with vowels - word beginning with e",
           "earay",
           translate("ear"))
test_equal("ay is added to words that start with vowels - word beginning with i",
           "iglooay",
           translate("igloo"))
test_equal("ay is added to words that start with vowels - word beginning with o",
           "objectay",
           translate("object"))
test_equal("ay is added to words that start with vowels - word beginning with u",
           "underay",
           translate("under"))
test_equal("ay is added to words that start with vowels - word beginning with a vowel and followed by a qu",
           "equalay",
           translate("equal"))
test_equal("first letter and ay are moved to the end of words that start with consonants - word beginning with p",
           "igpay",
           translate("pig"))
test_equal("first letter and ay are moved to the end of words that start with consonants - word beginning with k", 
           "oalakay",
           translate("koala"))
test_equal("first letter and ay are moved to the end of words that start with consonants - word beginning with x",
           "enonxay",
           translate("xenon"))
test_equal("first letter and ay are moved to the end of words that start with consonants - word beginning with q without a following u",
           "atqay",
           translate("qat"))
test_equal("some letter clusters are treated like a single consonant - word beginning with ch",
           "airchay",
           translate("chair"))
test_equal("some letter clusters are treated like a single consonant - word beginning with qu",
           "eenquay",
           translate("queen"))
test_equal("some letter clusters are treated like a single consonant - word beginning with qu and a preceding consonant",
           "aresquay",
           translate("square"))
test_equal("some letter clusters are treated like a single consonant - word beginning with th",
           "erapythay",
           translate("therapy"))
test_equal("some letter clusters are treated like a single consonant - word beginning with thr",
           "ushthray",
           translate("thrush"))
test_equal("some letter clusters are treated like a single consonant - word beginning with sch",
           "oolschay",
           translate("school"))
test_equal("some letter clusters are treated like a single vowel - word beginning with yt",
           "yttriaay",
           translate("yttria"))
test_equal("some letter clusters are treated like a single vowel - word beginning with xr",
           "xrayay",
           translate("xray"))
test_equal("position of y in a word determines if it is a consonant or a vowel - y is treated like a consonant at the beginning of a word",
           "ellowyay",
           translate("yellow"))
test_equal("position of y in a word determines if it is a consonant or a vowel - y is treated like a vowel at the end of a consonant cluster",
           "ythmrhay",
           translate("rhythm"))
test_equal("position of y in a word determines if it is a consonant or a vowel - y as second letter in two letter word",
           "ymay",
           translate("my"))
test_equal("phrases are translated - a whole phrase",
           "ickquay astfay unray",
           translate("quick fast run"))

test_report()
