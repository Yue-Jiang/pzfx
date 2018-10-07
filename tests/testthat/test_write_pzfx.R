context("write_pzfx")

test_that("Test writing 'Column' type table", {
  tmp <- tempfile(fileext=".pzfx")
  on.exit(unlink(tmp))
  expected_file <- system.file("testdata/column.tab", package="pzfx", mustWork=TRUE)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  write_pzfx(expected, tmp, row_names=FALSE)
  pzfx <- read_pzfx(tmp)
  expect_equal(pzfx, expected)
})

test_that("Test writing 'XY' type table", {
  tmp <- tempfile(fileext=".pzfx")
  on.exit(unlink(tmp))
  expected_file <- system.file("testdata/x_y_no_rep.tab", package="pzfx", mustWork=TRUE)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  to_write <- expected[, colnames(expected) != "ROWTITLE"]
  rownames(to_write) <- expected$ROWTITLE
  write_pzfx(to_write, tmp, row_names=TRUE, x_col="XX")
  pzfx <- read_pzfx(tmp)
  expect_equal(pzfx, expected)
})

test_that("Test multiple input tables work", {
  tmp <- tempfile(fileext=".pzfx")
  on.exit(unlink(tmp))
  expected_file <- system.file("testdata/column.tab", package="pzfx", mustWork=TRUE)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  to_write <- list("T1"=expected, "T2"=expected)
  write_pzfx(to_write, tmp, row_names=FALSE)
  pzfx1 <- read_pzfx(tmp, table="T1")
  pzfx2 <- read_pzfx(tmp, table=2)
  expect_equal(pzfx1, expected)
  expect_equal(pzfx2, expected)
})

test_that("Test writing matrix works", {
  tmp <- tempfile(fileext=".pzfx")
  on.exit(unlink(tmp))
  expected_file <- system.file("testdata/column.tab", package="pzfx", mustWork=TRUE)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  write_pzfx(as.matrix(expected), tmp, row_names=FALSE)
  pzfx <- read_pzfx(tmp)
  expect_equal(pzfx, expected)
})

test_that("Should raise when provided with wrong type of input", {
  tmp <- tempfile(fileext=".pzfx")
  on.exit(unlink(tmp))
  expect_error(write_pzfx(1:10, tmp, row_names=FALSE), "Cannot process x of class integer")
  expect_error(write_pzfx(rnorm(10), tmp, row_names=FALSE), "Cannot process x of class numeric")
  expect_error(write_pzfx("Existence is pain", tmp, row_names=FALSE),
               "Cannot process x of class character")
  expect_error(write_pzfx(data.frame("X"=c("a", "b"), "Y"=1:2), tmp, row_names=FALSE),
               "These tables are not all numeric: Data 1")
  expect_error(write_pzfx(list("a"=1:10), tmp, row_names=FALSE),
               "These elements are not data frame or matrix: a")
})

test_that("Should raise when provided with wrong 'x_col'", {
  tmp <- tempfile(fileext=".pzfx")
  on.exit(unlink(tmp))
  expect_error(write_pzfx(data.frame("SingleColumn"=1:10), tmp, x_col=2),
               "Not enough columns for table Data 1")
  expect_error(write_pzfx(list(data.frame(1:2), data.frame(3:4)), tmp, x_col=c(1, 1, 1)),
               "Argument 'x_col' can only be of length 1 or the length of 'x'")
})

test_that("Should raise when provided with wrong 'row_names'", {
  tmp <- tempfile(fileext=".pzfx")
  on.exit(unlink(tmp))
  expect_error(write_pzfx(list(data.frame(1:2), data.frame(3:4)), tmp, row_names=c(TRUE, FALSE, TRUE)),
               "Argument 'row_names' can only be of length 1 or the length of 'x'")
})
