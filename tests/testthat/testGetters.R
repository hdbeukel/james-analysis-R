context("Extract data")

# load data
data(james)

test_that("getProblems returns correct ordered problem names", {
  expect_equal(getProblems(james), c("coconut", "maize-accession", "maize-bulk", "pea-small"))
})

test_that("getProblems respects applied filter", {
  expect_equal(getProblems(james, filter = "maize"), c("maize-accession", "maize-bulk"))
})

test_that("getProblems passes additional arguments to grep", {
  expect_equal(getProblems(james, filter = "Coco"), character(0))
  expect_equal(getProblems(james, filter = "Coco", ignore.case = TRUE), "coconut")
})

test_that("getSearches returns correct ordered search names", {
  for(p in getProblems(james)){
    expect_equal(getSearches(james, p), c("Parallel Tempering", "Random Descent"))
  }
})

test_that("getSearches respects applied filter", {
  for(p in getProblems(james)){
    expect_equal(getSearches(james, p, filter = "Descent"), "Random Descent")
  }
})

test_that("getSearches passes additional arguments to grep", {
  for(p in getProblems(james)){
    expect_equal(getSearches(james, p, filter = "descent"), character(0))
    expect_equal(getSearches(james, p, filter = "descent", ignore.case = TRUE), "Random Descent")
  }
})

test_that("getSearches allows to ommit problem name only if the data addresses a single problem only", {
  expect_error(getSearches(james), "more than one problem")
  # extract coconut problem
  james.coco <- reduceJAMES(james, problems = "coco")
  expect_equal(getSearches(james.coco), c("Parallel Tempering", "Random Descent"))
})

test_that("getSearches complains if the given problem is not found", {
  expect_error(getSearches(james, "foo"), "does not contain results for problem")
})







