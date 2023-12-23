use context essentials2020

include file("space-age.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun around(delta :: Number) -> (Number, Number -> Boolean):
  doc: "provides a predicate that returns true if the absolute values of two numbers are less than or equal to the specified delta"
  lam(actual, target):
    num-abs(target - actual) <= delta
  end
end

fun age-on-earth():
  check "age on Earth":
    on-planet("Earth", 1000000000)  is%(around(0.01)) 31.69
  end
end

fun age-on-mercury():
  check "age on Mercury":
    on-planet("Mercury", 2134835688) is%(around(0.01)) 280.88
  end
end

fun age-on-venus():
  check "age on Venus":
    on-planet("Venus", 189839836) is%(around(0.01)) 9.78
  end
end

fun age-on-mars():
  check "age on Mars":
    on-planet("Mars", 2129871239) is%(around(0.01)) 35.88
  end
end

fun age-on-jupiter():
  check "age on Jupiter":
    on-planet("Jupiter", 901876382) is%(around(0.01)) 2.41
  end
end

fun age-on-saturn():
  check "age on Saturn":
    on-planet("Saturn", 2000000000) is%(around(0.01)) 2.15
  end
end

fun age-on-uranus():
  check "age on Uranus":
    on-planet("Uranus", 1210123456) is%(around(0.01)) 0.46
  end
end

fun age-on-neptune():
  check "age on Neptune":
    on-planet("Neptune", 1821023456) is%(around(0.01)) 0.35
  end
end

fun invalid-planet-causes-error():
  check "invalid planet causes error":
    on-planet("Sun", 680804807) raises "not a planet"
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(age-on-earth, true),
  test(age-on-mercury, true),
  test(age-on-venus, true),
  test(age-on-mars, true),
  test(age-on-jupiter, true),
  test(age-on-saturn, true),
  test(age-on-uranus, true),
  test(age-on-neptune, true),
  test(invalid-planet-causes-error, true)
].each(lam(t): when t.active: t.run() end end)
