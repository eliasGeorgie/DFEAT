#'
#' @title Check Yearly Day Counts and Starting Date
#' @description This function checks three conditions for a given input Daily climate dataset:
#' 1. Whether all years have exactly 365 or 366 days.
#' 2. Whether all years start on January 1st.
#' 3. Whether the year are in chronological order.
#' It prints messages indicating whether the conditions are met and provides details
#' on any discrepancies, including which years have fewer than 365 days and do not start on January 1st
#' @param data A daily climate data frame containing the columns `year`, `month`, and `day`.
#' The function assumes that these columns represent the year, month, and day for each observation in the dataset.
#' @return The function does not return a value, but prints messages to the console.
#' If all years have 365 or 366 days and start on January 1st, it prints a confirmation message.
#' If not, it prints which years are incomplete or do not start on January 1st. Also, if years are not
#' in a sequential order the function will stop and print a message about the displaced years.
#'
#' @examples
#' # Assuming 'tab_clim' is a dataset with 'year', 'month', and 'day' columns
#' check_years_days_and_start(tab_clim)
#'

check_years_days_and_start <- function(data) {

  # Check if the necessary columns exist in the data
  if (!all(c("year", "month", "day") %in% colnames(data))) {
    stop("The data must contain columns: 'year', 'month', and 'day'")
  }

  # Convert year, month, and day to numeric
  data$year <- as.numeric(data$year)
  data$month <- as.numeric(data$month)
  data$day <- as.numeric(data$day)

  # Check if years are in sequential order
  sorted_years <- unique(data$year)
  if (!all(diff(sorted_years) == 1)) {
    displaced_years <- sorted_years[which(diff(sorted_years) != 1) + 1]
    stop("The years are not in chronological order. Please check the following displaced years: ",
         paste(displaced_years, collapse = ", "))
  } else {
    # Print confirmation message if years are in chronological order
    message("The years are in chronological order")
  }

  # Count the number of distinct days per year
  year_counts <- data |>
    group_by(year) |>
    summarize(days = n_distinct(paste(month, day, sep = "-")))

  # Check if all years have 365 or 366 days
  if (all(year_counts$days == 365
          | (year_counts$days == 366
             & year_counts$year %% 4 == 0
             & year_counts$year %% 100 != 0)
          | (year_counts$days == 366
             & year_counts$year %% 400 == 0))) {
    message("All years have 365 or 366 days")
  } else {
    warning("Some years do not have 365 or 366 days")
    # Identify years that are not fully present (not 365 or 366 days)
    incomplete_years <- year_counts |>
      filter(!(days == 365
               | (days == 366
                  & year %% 4 == 0
                  & year %% 100 != 0)
               | (days == 366
                  & year %% 400 == 0)))
    # Output incomplete years
    message("The following years are not fully present (either 365 or 366 days):")
    print(incomplete_years$year)
  }

  # Check if each year starts on January 1st
  year_starts_jan_first <- data |>
    group_by(year) |>
    summarize(jan_first = sum(as.numeric(month == 1) & as.numeric(day == 1)))

  if (all(year_starts_jan_first$jan_first == 1)) {
    message("All years start on January 1st")
  } else {
    warning("Some years do not start on January 1st")

    # Identify years that do not start on January 1st
    non_jan_first_years <- year_starts_jan_first |>
      filter(jan_first == 0)

    # Output years that do not start on January 1st
    warning("The following years do not start on January 1st:")
    message(non_jan_first_years$year)
  }
}
