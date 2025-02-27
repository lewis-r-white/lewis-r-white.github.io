# library(dplyr)
# 
# summarize_pollution_times <- function(data, pollutant) {
#   mean_col <- paste0("mean_", pollutant)
#   fleet_col <- paste0("fleet_average_", pollutant)
#   
#   hourly_summary <- data %>%
#     group_by(monitor, date, hour) %>%
#     summarise(!!mean_col := mean(!!sym(pollutant), na.rm = TRUE), .groups = 'drop')
#   
#   daily_summary <- data %>%
#     group_by(monitor, date) %>%
#     summarise(!!mean_col := mean(!!sym(pollutant), na.rm = TRUE), .groups = 'drop')
#   
#   fleet_hourly <- hourly_summary %>%
#     group_by(date, hour) %>%
#     summarise(!!fleet_col := mean(!!sym(mean_col), na.rm = TRUE), .groups = 'drop')
#   
#   fleet_daily <- daily_summary %>%
#     group_by(date) %>%
#     summarise(!!fleet_col := mean(!!sym(mean_col), na.rm = TRUE), .groups = 'drop')
#   
#   hourly_full <- left_join(hourly_summary, fleet_hourly, by = c("date", "hour"))
#   daily_full <- left_join(daily_summary, fleet_daily, by = "date")
#   
#   list(hourly = hourly_full, daily = daily_full)
# }


summarize_pollution_times <- function(data, pollutant) {
  mean_col <- paste0("mean_", pollutant)
  fleet_col <- paste0("fleet_average_", pollutant)
  
  # Hourly summary with completeness check (45 valid minutely observations)
  hourly_summary <- data %>%
    group_by(monitor, date, hour) %>%
    summarise(
      n_minute_obs = sum(!is.na(!!sym(pollutant))),  # Count non-NA minute-level observations
      !!mean_col := ifelse(n_minute_obs >= 45, mean(!!sym(pollutant), na.rm = TRUE), NA_real_),  # Only calculate if 75% of minutes are non-NA
      .groups = 'drop'
    )
  
  # Daily summary with completeness check (18 completed hours)
  daily_summary <- hourly_summary %>%
    group_by(monitor, date) %>%
    summarise(
      n_complete_hours = sum(!is.na(!!sym(mean_col))),  # Count non-NA complete hours
      !!mean_col := ifelse(n_complete_hours >= 18, mean(!!sym(mean_col), na.rm = TRUE), NA_real_),  # Only calculate if 75% of hours are complete
      .groups = 'drop'
    )
  
  # Fleet summaries
  fleet_hourly <- hourly_summary %>%
    group_by(date, hour) %>%
    summarise(
      !!fleet_col := mean(!!sym(mean_col), na.rm = TRUE),
      .groups = 'drop'
    )
  
  fleet_daily <- daily_summary %>%
    group_by(date) %>%
    summarise(
      !!fleet_col := mean(!!sym(mean_col), na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Join fleet averages with individual summaries
  hourly_full <- left_join(hourly_summary, fleet_hourly, by = c("date", "hour"))
  daily_full <- left_join(daily_summary, fleet_daily, by = "date")
  
  list(hourly = hourly_full, daily = daily_full)
}

