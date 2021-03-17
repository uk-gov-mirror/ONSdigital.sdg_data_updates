test_that("get_modal_values returns expected values for numberic values", {
  expect_equal(get_modal_values(-1000), -1000)
  expect_equal(get_modal_values(0), 0)
  expect_equal(get_modal_values(1000), 1000)

  expect_equal(get_modal_values(rep(0, 5)), 0)
  expect_equal(get_modal_values(rep(-1000, 5)), -1000)
  expect_equal(get_modal_values(rep(1000, 5)), 1000)

  expect_equal(get_modal_values(c(rep(1, 2), 1000)), 1)
  expect_equal(get_modal_values(c(rep(1, 2), 1000, 1)), 1)
  expect_equal(get_modal_values(c(rep(1, 2), 1000, -1000)), 1)

  expect_equal(get_modal_values(c(1, rep(1000, 2), -1000)), 1000)
  expect_equal(get_modal_values(c(1, 1000, rep(-1000,2))), -1000)

  expect_equal(get_modal_values(c(rep(0, 5), rep(1000, 5))), c(0, 1000))
  expect_equal(get_modal_values(c(rep(-1000, 5), rep(1000, 5))), c(-1000, 1000))
})

test_that("get_modal_values returns expected values for character vectors", {
  expect_equal(get_modal_values("a"), "a")
  expect_equal(get_modal_values("0"), "0")
  expect_equal(get_modal_values("$"), "$")

  expect_equal(get_modal_values(rep("a", 5)), "a")
  expect_equal(get_modal_values(rep("0", 5)), "0")
  expect_equal(get_modal_values(rep("$", 5)), "$")

  expect_equal(get_modal_values(c(rep("a", 2), 1000)), "a")
  expect_equal(get_modal_values(c(rep("a", 2), 1000, "1")), "a")

  expect_equal(get_modal_values(c("a", rep(1000, 2))), "1000")
  expect_equal(get_modal_values(c(1000, rep("a",2))), "a")

  expect_equal(get_modal_values(c(rep("a", 5), rep("1000", 5))), c("a", "1000"))
})
