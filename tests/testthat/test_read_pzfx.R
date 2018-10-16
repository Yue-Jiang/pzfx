context("read_pzfx")

test_that("Test column", {
  pzfx_file <- system.file("testdata/column.pzfx", package="pzfx", mustWork=TRUE)
  expected_file <- system.file("testdata/column.tab", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
})

test_that("Test column CV", {
  pzfx_file <- system.file("testdata/column_cv.pzfx", package="pzfx", mustWork=TRUE)
  expected_file <- system.file("testdata/column_cv.tab", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
})

test_that("Test column CVN", {
  pzfx_file <- system.file("testdata/column_cvn.pzfx", package="pzfx", mustWork=TRUE)
  expected_file <- system.file("testdata/column_cvn.tab", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
})

test_that("Test column low-high", {
  pzfx_file <- system.file("testdata/column_low-high.pzfx", package="pzfx", mustWork=TRUE)
  expected_file <- system.file("testdata/column_low-high.tab", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
})

test_that("Test column SD", {
  pzfx_file <- system.file("testdata/column_sd.pzfx", package="pzfx", mustWork=TRUE)
  expected_file <- system.file("testdata/column_sd.tab", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
})

test_that("Test column SDN", {
  pzfx_file <- system.file("testdata/column_sdn.pzfx", package="pzfx", mustWork=TRUE)
  expected_file <- system.file("testdata/column_sdn.tab", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
})

test_that("Test column SE", {
  pzfx_file <- system.file("testdata/column_se.pzfx", package="pzfx", mustWork=TRUE)
  expected_file <- system.file("testdata/column_se.tab", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
})

test_that("Test column SEN", {
  pzfx_file <- system.file("testdata/column_sen.pzfx", package="pzfx", mustWork=TRUE)
  expected_file <- system.file("testdata/column_sen.tab", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
})

test_that("Test column upper lower limits", {
  pzfx_file <- system.file("testdata/column_upper-lower-limits.pzfx", package="pzfx", mustWork=TRUE)
  expected_file <- system.file("testdata/column_upper-lower-limits.tab", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
})

test_that("Test contingency table", {
  pzfx_file <- system.file("testdata/contingency.pzfx", package="pzfx", mustWork=TRUE)
  expected_file <- system.file("testdata/contingency.tab", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
})

test_that("Test X column with error bars", {
  pzfx_file <- system.file("testdata/x_error_y_sdn.pzfx", package="pzfx", mustWork=TRUE)
  expected_file <- system.file("testdata/x_error_y_sdn.tab", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
})

test_that("Test X-Y table without replicates", {
  pzfx_file <- system.file("testdata/x_y_no_rep.pzfx", package="pzfx", mustWork=TRUE)
  expected_file <- system.file("testdata/x_y_no_rep.tab", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
})

test_that("Test excluded values", {
  pzfx_file <- system.file("testdata/x_y_with_strike.pzfx", package="pzfx", mustWork=TRUE)
  # excluded
  pzfx <- read_pzfx(pzfx_file, strike_action="exclude")
  expected_file <- system.file("testdata/x_y_with_strike_excluded.tab", package="pzfx", mustWork=TRUE)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
  # kept
  pzfx <- read_pzfx(pzfx_file, strike_action="keep")
  expected_file <- system.file("testdata/x_y_with_strike_kept.tab", package="pzfx", mustWork=TRUE)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected, strike_action="star")
  # star
  pzfx <- read_pzfx(pzfx_file, strike_action="star")
  expected_file <- system.file("testdata/x_y_with_strike_star.tab", package="pzfx", mustWork=TRUE)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE,
                         colClasses="character")
  expect_equal(pzfx, expected)
})

test_that("Test survival data", {
  pzfx_file <- system.file("testdata/survival.pzfx", package="pzfx", mustWork=TRUE)
  expected_file <- system.file("testdata/survival.tab", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- read.table(expected_file, sep="\t", header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
})

test_that("Test parts of whole data", {
  pzfx_file <- system.file("testdata/parts_of_whole.pzfx", package="pzfx", mustWork=TRUE)
  expected_file <- system.file("testdata/parts_of_whole.tab", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- read.table(expected_file, sep="\t", header=TRUE, check.names=FALSE, comment.char="", stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
})

test_that("Test HugeTable", {
  pzfx_file <- system.file("testdata/column_hugetable.pzfx", package="pzfx", mustWork=TRUE)
  expected_file <- system.file("testdata/column_hugetable.tab", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
})

test_that("Test table with empty column", {
  pzfx_file <- system.file("testdata/column_empty.pzfx", package="pzfx", mustWork=TRUE)
  expected_file <- system.file("testdata/column_empty.tab", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
})

test_that("Test empty table", {
  pzfx_file <- system.file("testdata/empty.pzfx", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- data.frame()
  expect_equal(pzfx, expected)
})

test_that("Test columns with different lengths", {
  pzfx_file <- system.file("testdata/column_unequal_lengths.pzfx", package="pzfx", mustWork=TRUE)
  expected_file <- system.file("testdata/column_unequal_lengths.tab", package="pzfx", mustWork=TRUE)
  pzfx <- read_pzfx(pzfx_file)
  expected <- read.table(expected_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  expect_equal(pzfx, expected)
})

test_that("Should raise when table is absent", {
  pzfx_file <- system.file("testdata/parts_of_whole.pzfx", package="pzfx", mustWork=TRUE)
  expect_error(read_pzfx(pzfx_file, "WrongTab"), "Can't find WrongTab in prism file")
})
