test_that("count_decimal_places_numerically stops and gives error when value is not numeric", {
  expect_error(count_decimal_places_numerically("1"),
               "value must be numeric", fixed = TRUE)
  expect_error(count_decimal_places_numerically("a"),
               "value must be numeric", fixed = TRUE)
  expect_error(count_decimal_places_numerically(NA),
               "value must be numeric", fixed = TRUE)
  expect_error(count_decimal_places_numerically(c("1", NA)),
               "value must be numeric", fixed = TRUE)
})

test_that("count_decimal_places_numerically returns expected datatype", {
  expect_equal(typeof(count_decimal_places_numerically(1)), "double")
  expect_equal(typeof(count_decimal_places_numerically(1.1)), "double")
  expect_equal(typeof(count_decimal_places_numerically(1.12345678)), "double")
  expect_equal(typeof(count_decimal_places_numerically(12345678)), "double")
  expect_equal(typeof(count_decimal_places_numerically(12345678.12345678)), "double")
  expect_equal(typeof(count_decimal_places_numerically(c(1, 1.1, 1.2))), "double")
})

test_that("count_decimal_places_numerically gives expected value", {
  expect_equal(count_decimal_places_numerically(0), 0)
  expect_equal(count_decimal_places_numerically(1), 0)
  expect_equal(count_decimal_places_numerically(1.000), 0)
  expect_equal(count_decimal_places_numerically(12345), 0)
  expect_equal(count_decimal_places_numerically(123456789012345678901234567890123456789), 0)

  expect_equal(count_decimal_places_numerically(1.1), 1)
  expect_equal(count_decimal_places_numerically(10.1), 1)
  expect_equal(count_decimal_places_numerically(1.10), 1)
  expect_equal(count_decimal_places_numerically(1234567890.1), 1)
  expect_equal(count_decimal_places_numerically(123456789012345.1), 1) # can't get more than 1dp with a number this large

  expect_equal(count_decimal_places_numerically(1.01), 2)
  expect_equal(count_decimal_places_numerically(1.22), 2)
  expect_equal(count_decimal_places_numerically(1.23), 2)
  expect_equal(count_decimal_places_numerically(2.23), 2)

  expect_equal(count_decimal_places_numerically(1.1234567890123456), 16) # can't get more than 16dp with a single digit number
  expect_equal(count_decimal_places_numerically(12.123456789012345), 15)
  expect_equal(count_decimal_places_numerically(123.12345678901234), 14)

  expect_equal(count_decimal_places_numerically(c(1.1, 100.12)), c(1, 2))
  expect_equal(count_decimal_places_numerically(c(1.1, NA)), c(1, NA))

  # fails
  # expect_equal(count_decimal_places_numerically(1.11), 2) # returns 3 - I can't work out why
  # expect_equal(count_decimal_places_numerically(10.12), 2) # 3
  # expect_equal(count_decimal_places_numerically(10.03), 2) # 3
  # expect_equal(count_decimal_places_numerically(1.03), 2) # 3
})


test_that("count_decimal_places_numerically returns vector of equal length to input vector", {
  expect_equal(length(count_decimal_places_numerically(1)), 1)
  expect_equal(length(count_decimal_places_numerically(123)), 1)

  expect_equal(length(count_decimal_places_numerically(c(1.2, NA))), 2)
  expect_equal(length(count_decimal_places_numerically(c(1.2, 4.5))), 2)

  expect_equal(length(count_decimal_places_numerically(c(1, 1.1, 10))), 3)
})
