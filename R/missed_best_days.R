#' Impact of Missing the Best Days in the Market
#'
#' Simulates final portfolio value with and without the best return days.
#'
#' @param price_data A data frame with columns 'Date' and 'Price'.
#' @param n_days Number of best return days to remove.
#' @param initial_investment Starting investment amount.
#'
#' @return A data frame comparing end values, and a ggplot object.
#' @export
missed_best_days <- function(price_data, n_days = 10, initial_investment = 10000) {
  library(dplyr)
  library(ggplot2)

  price_data <- price_data %>%
    arrange(Date) %>%
    mutate(Return = Price / lag(Price) - 1) %>%
    filter(!is.na(Return))

  # Full investment
  full_growth <- cumprod(1 + price_data$Return) * initial_investment

  # Remove best n return days
  top_days <- price_data %>%
    slice_max(order_by = Return, n = n_days)

  reduced_data <- price_data %>%
    filter(!Date %in% top_days$Date)

  reduced_growth <- cumprod(1 + reduced_data$Return) * initial_investment

  # Prepare result summary
  summary_df <- data.frame(
    Scenario = c("All Days", paste("Missed Best", n_days, "Days")),
    Final_Value = round(c(tail(full_growth, 1), tail(reduced_growth, 1)), 2)
  )

  # Plot
  full_plot <- data.frame(Date = price_data$Date, Value = full_growth)
  reduced_plot <- data.frame(Date = reduced_data$Date, Value = reduced_growth)
  full_plot$Scenario <- "All Days"
  reduced_plot$Scenario <- paste("Missed Best", n_days, "Days")

  combined <- rbind(full_plot, reduced_plot)

  p <- ggplot(combined, aes(x = Date, y = Value, color = Scenario)) +
    geom_line() +
    labs(title = "Effect of Missing the Best Days in the Market",
         y = "Portfolio Value", x = "Date") +
    theme_minimal()

  print(summary_df)
  return(p)
}
