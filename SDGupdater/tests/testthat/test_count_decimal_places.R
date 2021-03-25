test_that("count_decimal_places stops and gives error when value is not numeric", {
  expect_error(count_decimal_places("1"),
               "value must be numeric", fixed = TRUE)
  expect_error(count_decimal_places("a"),
               "value must be numeric", fixed = TRUE)
  expect_error(count_decimal_places(NA),
               "value must be numeric", fixed = TRUE)
  expect_error(count_decimal_places(c("1", NA)),
               "value must be numeric", fixed = TRUE)
})

test_that("count_decimal_places returns expected datatype", {
  expect_equal(typeof(count_decimal_places(1)), "double")
  expect_equal(typeof(count_decimal_places(1.1)), "double")
  expect_equal(typeof(count_decimal_places(1.12345678)), "double")
  expect_equal(typeof(count_decimal_places(12345678)), "double")
  expect_equal(typeof(count_decimal_places(12345678.1234567)), "double")
  expect_equal(typeof(count_decimal_places(c(1.1, 1.2))), "double")
  expect_equal(typeof(count_decimal_places(c(123456789012345678901,5, rep(10.3,2)))), "double") # where NA values are almost as common as real counts
})

test_that("count_decimal_places gives warning when default count returned", {
  expect_warning(count_decimal_places(123456789012345678901),
               "Unable to count decimal places, please check returned number of decimal places is acceptable and manually adjust if not.", fixed = TRUE)
  expect_warning(count_decimal_places(rep(123456789012345678901.2,2)),
               "Unable to count decimal places, please check returned number of decimal places is acceptable and manually adjust if not.", fixed = TRUE)

  # # the following case returns 0, which is a fail - caused by truncation
  # # at the decimal. However, this function will usually be used on a vector >
  # # length 1, so it will be very rare that this is an issue. Note added to
  # # documentation.
  # expect_error(count_decimal_places(12345678901234567890.1),
  #              "Unable to count decimal places, please check returned number of decimal places is acceptable and manually adjust if not.", fixed = TRUE)

})

# test_that("count_decimal_places gives correct warning when return value is unreliable", {
# expect_equal(typeof(count_decimal_places(pi)), "double")
  #   expect_equal(count_decimal_places(123456789012345.1), 1) # can't get more than 1dp with a number this large
  #   expect_equal(count_decimal_places(1.1234567890123456), 16) # can't get more than 16dp with a single digit number
  #   expect_equal(count_decimal_places(12.123456789012345), 15)
  #   expect_equal(count_decimal_places(123.12345678901234), 14)
  #   expect_equal(count_decimal_places(1.11), 2) # returns 3 - I can't work out why
  #   expect_equal(count_decimal_places(10.03), 2) # 3
# })

test_that("count_decimal_places gives expected value", {
  expect_equal(count_decimal_places(0), 0)
  expect_equal(count_decimal_places(1), 0)
  expect_equal(count_decimal_places(1.000), 0)
  expect_equal(count_decimal_places(12345), 0)

  expect_equal(count_decimal_places(1.1), 1)
  expect_equal(count_decimal_places(10.1), 1)
  expect_equal(count_decimal_places(1.10), 1)
  expect_equal(count_decimal_places(1234567890.1), 1)

  expect_equal(count_decimal_places(1.01), 2)
  expect_equal(count_decimal_places(1.22), 2)
  expect_equal(count_decimal_places(1.23), 2)
  expect_equal(count_decimal_places(2.23), 2)

  expect_equal(count_decimal_places(c(1.1, NA)), 1)
  expect_equal(count_decimal_places(c(1.1, 100.12)), 2)
  expect_equal(count_decimal_places(c(1.1, NA)), 1)
})
