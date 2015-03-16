context("Extract data")

# load data
data(james)

test_that("getProblems returns correct, ordered problem names", {
  expect_equal(getProblems(james), c("coconut", "maize-accession", "maize-bulk", "pea-small"))
})

test_that("getProblems respects applied filter", {
  expect_equal(getProblems(james, filter = "maize"), c("maize-accession", "maize-bulk"))
})

test_that("getProblems passes additional arguments to grep", {
  expect_equal(getProblems(james, filter = "Coco"), character(0))
  expect_equal(getProblems(james, filter = "Coco", ignore.case = TRUE), "coconut")
})




