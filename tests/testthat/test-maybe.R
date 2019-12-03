context("maybe pipe")

test_that("Join rules are obeyed", {
  a <- 5
  msg <- "error message"

  expect_identical(just(just(a)), just(a))
  expect_identical(nothing(just(a)), nothing())
  expect_identical(just(nothing(msg)), nothing(msg))
  expect_identical(nothing(nothing(msg)), nothing(msg))
})

test_that("Values are unwrapped correctly", {
  a <- 5
  ja <- just(a)
  na <- nothing()

  expect_identical(unwrap(ja), a)
  expect_error(unwrap(na))
  expect_identical(unwrap(na, .default = NULL), NULL)
  
  # Concrete types don't need unwrapping
  expect_identical(unwrap(5), 5)
  expect_identical(unwrap(list(a = 1, b = 2, c = 3)), list(a = 1, b = 2, c = 3))
})

test_that("Simple pipe behavior make sense", {
  expect_identical(1:5 %?>% sum(), just(sum(1:5)))
  expect_identical(1:5 %?>% sum() %>% unwrap(), sum(1:5))

  bad_file <- file.path(system.file(package = "magrittr"), "file_that_does_not_exist.csv")
  output <- suppressWarnings({
      bad_file %?>% 
        read.csv() %?>%
        dplyr::filter(mpg < 20)
  })

  expect_true(is_nothing(output))
})

test_that("Subtle behavior is maintained", {
  expect_true(nothing() %>% is_nothing())
  expect_identical(nothing() %?>% is_nothing(), nothing())
})
