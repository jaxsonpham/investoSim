test_that("simulate_growth returns correct dimensions", {
  result <- simulate_growth(initial_investment = 1000, years = 2, annual_return = 0.06)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 24)  # 2 years * 12 months
})

test_that("simulate_growth handles zero monthly contribution", {
  result <- simulate_growth(initial_investment = 1000, years = 1, annual_return = 0.06, monthly_contribution = 0)

  expect_equal(result$With_Growth[1], round(1000 * (1 + (1 + 0.06)^(1/12) - 1), 2))
})
