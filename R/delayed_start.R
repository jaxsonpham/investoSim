#' Compare Investing Now vs. Delayed Start
#'
#' Compares ending portfolio value if you start investing now vs. delaying for X years.
#'
#' @param monthly_contribution Amount contributed monthly.
#' @param years Total investment time span.
#' @param delay_years Years delayed before starting.
#' @param annual_return Expected annual return rate (e.g., 0.07).
#'
#' @return A ggplot comparing both strategies.
#' @export
delayed_start <- function(monthly_contribution, years, delay_years, annual_return) {
  months <- years * 12
  delay_months <- delay_years * 12
  monthly_rate <- (1 + annual_return)^(1/12) - 1

  now <- numeric(months)
  delayed <- numeric(months)

  for (m in 1:months) {
    if (m > delay_months) {
      delayed[m] <- (delayed[m - 1] + monthly_contribution) * (1 + monthly_rate)
    } else if (m > 1) {
      delayed[m] <- delayed[m - 1] * (1 + monthly_rate)
    }

    if (m == 1) {
      now[m] <- monthly_contribution * (1 + monthly_rate)
    } else {
      now[m] <- (now[m - 1] + monthly_contribution) * (1 + monthly_rate)
    }
  }

  df <- data.frame(
    Month = 1:months,
    Invest_Now = now,
    Delayed = delayed
  )

  df_long <- reshape2::melt(df, id.vars = "Month", variable.name = "Strategy", value.name = "Value")

  ggplot(df_long, aes(x = Month, y = Value, color = Strategy)) +
    geom_line() +
    labs(title = "Impact of Delaying Your Investing",
         x = "Month", y = "Portfolio Value") +
    theme_minimal()
}
