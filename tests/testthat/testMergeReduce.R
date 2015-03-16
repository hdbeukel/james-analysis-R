context("Merge and reduce JSON files")

# load data
data(james)

test_that("merge after disjunct reduces yields original data", {
  # split PT and RD search results
  rd.data <- reduceJAMES(james, searches = "Descent")
  pt.data <- reduceJAMES(james, searches = "Tempering")
  # merge and compare with original data
  merged <- mergeJAMES(rd.data, pt.data)
  james.summary <- paste(capture.output(summary(james)), collapse = "\n")
  expect_output(summary(merged), james.summary)
})