#' Compare Lump Sum vs Dollar-Cost Averaging (DCA)
#'
#' This function compares the value over time of a lump sum investment
#' and a monthly dollar-cost averaging strategy over the same period.
#'
#' @param initial_investment Numeric. One-time lump sum investment.
#' @param monthly_contribution Numeric. Monthly amount contributed for DCA.
#' @param years Numeric. Investment duration in years.
#' @param annual_return Numeric. Expected annual return rate (e.g., 0.07 for 7%).
#'
#' @return A ggplot comparing both investment strategies over time.
#' @export
compare_strategies <- function(initial_investment, monthly_contribution, years, annual_return) {
  library(ggplot2)

  months <- years * 12
  monthly_return <- (1 + annual_return)^(1/12) - 1

  # Lump Sum
  lump_sum <- numeric(months)
  lump_sum[1] <- initial_investment * (1 + monthly_return)
  for (i in 2:months) {
    lump_sum[i] <- lump_sum[i - 1] * (1 + monthly_return)
  }

  # DCA
  dca <- numeric(months)
  dca[1] <- monthly_contribution * (1 + monthly_return)
  for (i in 2:months) {
    dca[i] <- (dca[i - 1] + monthly_contribution) * (1 + monthly_return)
  }

  df <- data.frame(
    Month = 1:months,
    Lump_Sum = round(lump_sum, 2),
    DCA = round(dca, 2)
  )

  df_long <- reshape2::melt(df, id.vars = "Month", variable.name = "Strategy", value.name = "Value")

  ggplot(df_long, aes(x = Month, y = Value, color = Strategy)) +
    geom_line(linewidth = 1.2) +
    labs(title = "Lump Sum vs Dollar-Cost Averaging",
         x = "Month", y = "Portfolio Value") +
    theme_minimal()
}
