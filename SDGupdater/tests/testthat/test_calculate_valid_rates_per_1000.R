
test_that("calculate_valid_rates_per_1000 stops with error when numerator is invalid", {
  expect_error(calculate_valid_rates_per_1000("10", 100, 2),
               "numerator must be numeric", fixed = TRUE)
  expect_error(calculate_valid_rates_per_1000("a", 100, 2),
               "numerator must be numeric", fixed = TRUE)
  expect_error(calculate_valid_rates_per_1000(NA, 100, 2),
               "numerator must be numeric", fixed = TRUE)
  expect_error(calculate_valid_rates_per_1000(c("a", 10), c(50, 100), 2),
               "numerator must be numeric", fixed = TRUE)
  expect_error(calculate_valid_rates_per_1000(c(10, 20), 100, 2),
                "numerator and denominator vectors must be of the same length", fixed = TRUE)
})

test_that("calculate_valid_rates_per_1000 stops with error when numerator is larger than denominator", {
  expect_error(calculate_valid_rates_per_1000(100, 10, 2),
               "numerator must be smaller than or equal to denominator")
  expect_error(calculate_valid_rates_per_1000(11, 10, 2),
               "numerator must be smaller than or equal to denominator")
  expect_error(calculate_valid_rates_per_1000(10.0000001, 10, 2),
               "numerator must be smaller than or equal to denominator")
})

test_that("calculate_valid_rates_per_1000 produces the correct value", {
  expect_equal(calculate_valid_rates_per_1000(10, 10, 0), 1000)

  expect_equal(calculate_valid_rates_per_1000(10, 1000, 0), 10)
  expect_equal(calculate_valid_rates_per_1000(10, 10000, 0), 1)
  expect_equal(calculate_valid_rates_per_1000(10, 100000, 0), 0)

  expect_equal(calculate_valid_rates_per_1000(10, 1000, 1), 10.0)
  expect_equal(calculate_valid_rates_per_1000(10, 10000, 1), 1.0)
  expect_equal(calculate_valid_rates_per_1000(10, 100000, 1), 0.1)

  expect_equal(calculate_valid_rates_per_1000(10.123456789, 1000, 0), 10)
  expect_equal(calculate_valid_rates_per_1000(10.19, 1000, 1), 10.2)
  expect_equal(calculate_valid_rates_per_1000(10.11, 1000, 1), 10.1)
  expect_equal(calculate_valid_rates_per_1000(10.123456789, 1000, 8), 10.12345679)
  expect_equal(calculate_valid_rates_per_1000(10, 1000, 8), 10)

  expect_equal(calculate_valid_rates_per_1000(3.01, 1000, 2), 3.01)
  expect_equal(calculate_valid_rates_per_1000(10.1, 1000, 2), 10.10)
  })

test_that("calculate_valid_rates_per_1000 returns NA when numerator is <= 3 but >= 0", {
  expect_equal(calculate_valid_rates_per_1000(0, 10, 0), NA)
  expect_equal(calculate_valid_rates_per_1000(1, 10, 0), NA)
  expect_equal(calculate_valid_rates_per_1000(2, 10, 0), NA)
  expect_equal(calculate_valid_rates_per_1000(3, 10, 0), NA)
})

test_that("calculate_valid_rates_per_1000 returns expected datatype", {
  expect_equal(typeof(calculate_valid_rates_per_1000(10, 10, 0)), "double")
  expect_equal(typeof(calculate_valid_rates_per_1000(10, 100000, 1)), "double")
  expect_equal(typeof(calculate_valid_rates_per_1000(0, 10, 0)), "logical")
  })



