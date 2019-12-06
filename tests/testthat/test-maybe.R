context("maybe pipe")

test_that("Join rules are obeyed", {
  a <- 5
  msg <- "error message"

  expect_identical(just(just(a)), just(a))
  expect_identical(nothing(just(a)), nothing())
  expect_identical(just(nothing(message = msg)), nothing(message = msg))
  expect_identical(nothing(nothing(message = msg)), nothing(message = msg))
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

  expect_true(is.nothing(output))
})

test_that("Composition of Maybe objects via maybe pipe makes sense", {
  a <- 5
  msg <- "Oops"
  
  expect_identical(just(a) %?>% nothing(), nothing())
  expect_identical(nothing() %?>% just(), nothing())
  expect_identical(just(a) %?>% just(), just(a))
  expect_identical(nothing(msg) %?>% nothing(), nothing(msg))
})

test_that("Subtle behavior is maintained", {
  a <- 5
  
  expect_true(nothing() %>% is.nothing())
  expect_identical(nothing() %?>% is.nothing(), nothing())
})
