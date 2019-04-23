source("./hello-world.R")
library(testthat)

context("hello world")

test_that("no name", {
  expect_equal(hello_world(), "Hello, World!")
})

test_that("a name", {
  expect_equal(hello_world("Joe"), "Hello, Joe!")
})

message("All tests passed for exercise: hello-world")
