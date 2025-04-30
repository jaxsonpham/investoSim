test_that("missed_best_days returns a plot without error", {
  expect_error(
    missed_best_days(
      price_data = fake_prices,
      n_days = 10,
      initial_investment = 10000
    ),
    regexp = NA
  )
})
