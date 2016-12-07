context("dec2base")

test_that("dec2base works with base 3", {
  expect_equal(dec2base(0:10, 3),
               c("0", "1", "2", "10", "11", "12", "20",
                 "21", "22", "100", "101"))
})

test_that("dec2base works with base 16", {
  expect_equal(dec2base(0:20, 16),
               c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B",
                 "C", "D", "E", "F", "10", "11", "12", "13", "14"))
})

test_that("dec2base works with arbitrary symbols", {
  expect_equal(dec2base(0:10, c("!@#")),
               c("!", "@", "#", "@!", "@@", "@#",
                 "#!", "#@", "##", "@!!", "@!@"))
})

test_that("dec2base throws correct errors if negative or zero base", {
  expect_error(dec2base(5, -1))
  expect_error(dec2base(5, 0))
})

test_that("dec2base throws correct errors if not number", {
  expect_error(dec2base(5, c(5, 2)))
})

test_that("dec2base throws error if base is a zero-length character vector", {
  expect_error(dec2base(5, character()))
})

test_that("dec2base throws error if base is integer and greater than 62", {

})

test_that("dec2base works when drop_leading=FALSE", {
  expect_equal(dec2base(0:10, 3, drop_leading = FALSE),
                c("000", "001", "002", "010", "011", "012", "020", "021", "022",
                  "100", "101"))
})

test_that("dec2alpha works when lower = TRUE", {
  expect_equal(dec2alpha(0:53, lower = TRUE), c(LETTERS, letters, "BA", "BB"))
})

test_that("dec2alpha works when lower = FALSE", {
  expect_equal(dec2alpha(0:27, lower = FALSE), c(LETTERS, "BA", "BB"))
})

test_that("dec2alpha works with drop_leading = FALSE", {
  expect_equal(dec2alpha(0:26, drop_leading = FALSE),
               c(str_c("A", LETTERS), "BA"))
})

test_that("dec2alpha works with width > 1", {
  expect_equal(dec2base(0:5, 2, width = 3),
               c("000", "001", "010", "011", "100", "101"))
})

test_that("dec2alpha with width > 1 doesn't truncate longer values", {
  expect_equal(dec2base(c(1, 100), 3, width = 3), c("001", "10201"))
})
