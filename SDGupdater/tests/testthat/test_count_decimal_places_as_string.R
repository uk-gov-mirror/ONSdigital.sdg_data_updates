test_that("count_decimal_places_as_string stops and gives error when value is not numeric", {
  expect_error(count_decimal_places_as_string("a"),
               "value must be numeric", fixed = TRUE)
  expect_error(count_decimal_places_as_string("a1"),
               "value must be numeric", fixed = TRUE)
  expect_error(count_decimal_places_as_string("1a"),
               "value must be numeric", fixed = TRUE)
  expect_error(count_decimal_places_as_string("1.0"),
               "value must be numeric", fixed = TRUE)
  expect_error(count_decimal_places_as_string(c("NA", 1)),
               "value must be numeric", fixed = TRUE)

})

test_that("count_decimal_places_as_string returns expected datatype", {
  expect_equal(typeof(count_decimal_places_as_string(1)), "integer")
  expect_equal(typeof(count_decimal_places_as_string(1.1)), "integer")
  expect_equal(typeof(count_decimal_places_as_string(1.12345678)), "integer")
  expect_equal(typeof(count_decimal_places_as_string(12345678)), "integer")
  expect_equal(typeof(count_decimal_places_as_string(12345678.12345678)), "integer")
  expect_equal(typeof(count_decimal_places_as_string(c(1, 1.1, 1.2))), "integer")
})

test_that("count_decimal_places_as_string gives expected value for numbers", {
  expect_equal(count_decimal_places_as_string(0), 0)
  expect_equal(count_decimal_places_as_string(1), 0)
  expect_equal(count_decimal_places_as_string(1.000), 0)
  expect_equal(count_decimal_places_as_string(12345), 0)

  expect_equal(count_decimal_places_as_string(123456789012345678901234567890123456789), NA)

  expect_equal(count_decimal_places_as_string(1.1), 1)
  expect_equal(count_decimal_places_as_string(10.1), 1)
  expect_equal(count_decimal_places_as_string(1.10), 1)
  expect_equal(count_decimal_places_as_string(1234567890.1), 1)
  expect_equal(count_decimal_places_as_string(12345678901234.1), 1) # can't get more than 1dp with a number this large

  expect_equal(count_decimal_places_as_string(1.01), 2)
  expect_equal(count_decimal_places_as_string(1.22), 2)
  expect_equal(count_decimal_places_as_string(1.23), 2)
  expect_equal(count_decimal_places_as_string(2.23), 2)

  expect_equal(count_decimal_places_as_string(1.12345678901234), 14) # can't get more than 14dp with a single digit number
  expect_equal(count_decimal_places_as_string(12.1234567890123), 13)
  expect_equal(count_decimal_places_as_string(123.123456789012), 12)

  expect_equal(count_decimal_places_as_string(c(1.1, 100.12)), c(1, 2))
  expect_equal(count_decimal_places_as_string(c(1.1, NA)), c(1, NA))

  expect_equal(count_decimal_places_as_string(1.11), 2)
  expect_equal(count_decimal_places_as_string(10.12), 2)
  expect_equal(count_decimal_places_as_string(10.03), 2)
  expect_equal(count_decimal_places_as_string(1.03), 2)
})


test_that("count_decimal_places_as_string returns vector of equal length to input vector", {
  expect_equal(length(count_decimal_places_as_string(1)), 1)
  expect_equal(length(count_decimal_places_as_string(123)), 1)

  expect_equal(length(count_decimal_places_as_string(c(1.2, NA))), 2)
  expect_equal(length(count_decimal_places_as_string(c(1.2, 4.5))), 2)

  expect_equal(length(count_decimal_places_as_string(c(1, 1.1, 10))), 3)
})
