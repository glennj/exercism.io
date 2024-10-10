include std/unittest.e

include pythagorean-triplets.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("triplets whose sum is 12",pythagorean_triplets(12),{{3,4,5}})
test_equal("triplets whose sum is 108",pythagorean_triplets(108),{{27,36,45}})
test_equal("triplets whose sum is 1000",pythagorean_triplets(1000),{{200,375,425}})
test_equal("no matching triplets for 1001",pythagorean_triplets(1001),{})
test_equal("several matching triplets",pythagorean_triplets(840),{{40,399,401},
  {56,390,394},
  {105,360,375},
  {120,350,370},
  {140,336,364},
  {168,315,357},
  {210,280,350},
  {240,252,348}})
test_equal("triplets for large number",pythagorean_triplets(30000),{{1200,14375,14425},
  {1875,14000,14125},
  {5000,12000,13000},
  {6000,11250,12750},
  {7500,10000,12500}})
test_equal("returns all matching triplets",{{9,40,41},{15,36,39}}, pythagorean_triplets(90))

test_report()
