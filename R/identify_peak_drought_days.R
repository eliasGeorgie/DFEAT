#' @title Identify Peak Drought Intensity Day (DOY) and its Maximum KBDI Value for Each Year
#'
#' @description
#' This function identifies the day of year (DOY) with the highest KBDI value (Peak.KBDI.DOY)
#' for each considered year, focusing on the period from 1st May to 31st December (period of soil moisture drought propagation under Mediterranean conditions). It also returns
#' the corresponding peak KBDI value for that day (Peak.KBDI). The function assumes that the input data contains complete and successive
#' daily 'KBDI' values with associated 'year', 'month', and 'day' columns.
#' The result is returned as a data frame containing the year, peak drought day (Peak.KBDI.DOY), and peak KBDI value (Peak.KBDI).
#' If any year is missing days, or if a fully present year contains missing KBDI values, the function will stop and notify the user with a warning about the affected years.
#' @param kbdi_data A data frame containing the daily calculated KBDI all over targeted years, with columns for 'year', 'month', and 'day'.
#' This data frame is primarily the output of the `calculate_KBDI` function and includes the following columns:
#' - \code{year}: Year of the observation.
#' - \code{month}: Month of the observation.
#' - \code{day}: Day of the observation.
#' - \code{KBDI}: The KBDI value for each day.
#'
#' The data should be structured with daily observations, and the columns must be named exactly as described.
#'
#' @return A data frame with the following columns:
#' - \code{year}: The year of the observation.
#' - \code{Peak.KBDI.DOY}: The day of year (DOY) of the peak KBDI value .
#' - \code{Peak.KBDI}: The peak KBDI value for that specific peak drought day, truncated to an integer value.
#'
#' @details
#' The function calculates the peak KBDI value and the corresponding day of year (DOY) for each year.
#' The function assumes that the input data frame is structured by day and includes the required columns for `year`, `month`, `day`, and `KBDI`.
#'
#' @examples
#' # Assuming `result_kbdi` is the output of the `calculate_KBDI` function:
#' peak_drought_info <- identify_peak_drought_days(result_kbdi)
#' head(peak_drought_info)
#'

identify_peak_drought_days <- function(kbdi_data) {

  # Ensure that the necessary columns exist
  if (!all(c("year", "month", "day", "KBDI") %in% colnames(kbdi_data))) {
    stop("The data must contain columns: 'year', 'month', 'day', and 'KBDI'")
  }

  # Initialize vectors to store peak KBDI day (DOY) and peak KBDI value for each year
  number_of_years <- length(unique(kbdi_data$year))
  peak_KBDI_DOY <- integer(number_of_years)
  peak_KBDI <- numeric(number_of_years)

  # Convert columns to numeric if necessary
  kbdi_data$year <- as.numeric(kbdi_data$year)
  kbdi_data$month <- as.numeric(kbdi_data$month)
  kbdi_data$day <- as.numeric(kbdi_data$day)

  # Count the number of days for each year
  year_counts <- kbdi_data %>%
    group_by(year) %>%
    summarize(days = n_distinct(paste(month, day, sep = "-")))

  # Check if any years are missing days (not fully present)
  incomplete_years <- year_counts %>%
    filter(!(days == 365 | (days == 366 & year %% 4 == 0 & year %% 100 != 0) | (days == 366 & year %% 400 == 0)))

  # If incomplete years are found, print a message
  if (nrow(incomplete_years) > 0) {
    warning_message <- paste(
      "The following years are not fully present:",
      paste(incomplete_years$year, collapse = ", "))
    warning(warning_message)

    # Stop the calculation and inform the user
    stop("Please check the data for missing days and incomplete years.")
  }


  # Additional check: Look for NA values in KBDI column for each year
  na_years <- kbdi_data %>%
    group_by(year) %>%
    summarize(na_count = sum(is.na(KBDI))) %>%
    filter(na_count > 0)

  # If NA values are found, print a message and stop the function
  if (nrow(na_years) > 0) {
    na_warning_message <- paste(
      "The KBDI column contains missing values (NA) in the following years:",
      paste(na_years$year, collapse = ", ")
    )
    warning(na_warning_message)

    # Stop the calculation and inform the user
    stop("Please check the KBDI column for missing values (NA).")
  }


  # Loop over each year to identify the peak KBDI value and its corresponding DOY
  for (i in 1:number_of_years) {
    # Filter data for the current year
    year_data <- kbdi_data[kbdi_data$year == unique(kbdi_data$year)[i], ]

    # Calculate Day of Year (DOY) for each day in the year
    year_data$DOY <- as.numeric(format(as.Date(paste(year_data$year, year_data$month, year_data$day, sep = "-")), "%j"))

    # Subset the data from DOY 121 (1st May) to DOY 365 (31st December) to focus on the drought period under Mediterranean conditions
    peak_drought <- year_data$KBDI[year_data$DOY >= 121 & year_data$DOY <= 365]

    # Find the maximum KBDI and its corresponding DOY within this period
    D_max <- max(peak_drought, na.rm = TRUE)
    D_max_DOY <- year_data$DOY[which(year_data$KBDI == D_max & year_data$DOY >= 121 & year_data$DOY <= 365)]

    # Store the results
    peak_KBDI_DOY[i] <- D_max_DOY
    peak_KBDI[i] <- D_max
  }

  # Create a result data frame with Peak DOY and Peak KBDI values
  result <- data.frame(
    year = unique(kbdi_data$year),
    Peak.KBDI.DOY = peak_KBDI_DOY,
    Peak.KBDI = trunc(peak_KBDI)
  )

  return(result)
}
