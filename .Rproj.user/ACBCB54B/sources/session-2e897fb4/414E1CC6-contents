#' Simulate Investment Growth Over Time with and without Interest
#'
#' Simulates the compounded investment value and compares it to simple savings (no growth).
#'
#' @param initial_investment Numeric. Starting investment amount.
#' @param years Numeric. Number of years invested.
#' @param annual_return Numeric. Annual return rate (e.g., 0.07 for 7%).
#' @param monthly_contribution Numeric. Monthly contribution amount. Default is 0.
#'
#' @return A data frame with month, compounded value, and no-growth (savings) value.
#' @export
simulate_growth <- function(initial_investment, years, annual_return, monthly_contribution = 0) {
  months <- years * 12
  monthly_rate <- (1 + annual_return)^(1/12) - 1

  compounding <- numeric(months)
  savings <- numeric(months)

  for (month in 1:months) {
    # Compound lump sum
    lump_sum_value <- initial_investment * (1 + monthly_rate)^month

    # Compound monthly contributions
    contrib_total <- 0
    for (m in 1:month) {
      contrib_total <- contrib_total + monthly_contribution * (1 + monthly_rate)^(month - m)
    }

    compounding[month] <- lump_sum_value + contrib_total
    savings[month] <- initial_investment + monthly_contribution * month
  }

  data.frame(
    Month = 1:months,
    With_Growth = round(compounding, 2),
    No_Growth = round(savings, 2)
  )
}
