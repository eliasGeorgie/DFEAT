
#' @title Initialize Empty Drought Features Table
#' @description Creates an empty table for storing extracted drought features from daily calculated KBDI data for the considered years.
#' Users should ensure that the input data frame for this function contains all years fully represented and arranged in chronological order.
#' Additionally, this function excludes the last year in the provided data frame;
#' for instance, if daily KBDI values span from 1960 to 2020, the year 2020 will be excluded.
#' This exclusion occurs because drought features for 2020 cannot be extracted reliably without data from 2021, as the subsequent year's information is required for accurate calculation.
#' @param kbdi_data A data frame containing daily KBDI data, including a 'year' column, which is the output of the `calculate_KBDI` function
#' @return A data frame with drought feature columns initialized for each hydrological year that will be characterized.
#' @examples
#' # Initialize an empty table for drought feature extraction
#' drought_features_table  <- initialize_drought_features_table(result_kbdi)
#' head(drought_features_table)


initialize_drought_features_table <- function(kbdi_data) {


  # Calculate the number of years in the dataset and exclude the last year due to calculation requirement
  number_of_years <- floor(nrow(kbdi_data) / 365.25) - 1

  # Determine the start year from the first entry in the 'year' column of KBDI data
  start_year <- kbdi_data$year[1]

  # Initialize an empty data frame with appropriate columns for drought features
  drought_features_table <- as.data.frame(matrix(NA, nrow = number_of_years, ncol = 21))

  # Set column names for drought features
  colnames(drought_features_table) <- c(
    'Drying.rate', 'Wetting.rate', 'Intercept.drying', 'Intercept.wetting', 'Peak.KBDI',
    'Peak.KBDI.DOY', 'RP.num', 'RP.I.mean', 'RP.sd', 'Low.D.Onset', 'Mod.D.Onset',
    'Extreme.D.Onset', 'Low.D.Offset', 'Mod.D.Offset', 'Extreme.D.Offset',
    'Low.D.Duration', 'Mod.D.Duration', 'Extreme.D.Duration',
    'Low.D.S', 'Mod.D.S', 'Extreme.D.S'
  )

  # Set row names for each hydrological year
  rownames(drought_features_table) <- paste("Hydro.Year.", start_year:(start_year + number_of_years - 1), sep = "")

  return(drought_features_table)
}
