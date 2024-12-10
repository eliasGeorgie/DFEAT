#' @title Multi-Year Drought (MYD) Missing Drought Features Characterization
#' @description This function takes as primary inputs the extracted drought features table, generated using the
#' `Drought_features_extraction function`, along with the kbdi_data calculated from the
#' `calculate_KBDI` function, and the outputs from the `identify_min_kbdi_between_two_drought_peaks`function.
#' Together, these inputs are used to re-calculate missing drought features for prolonged Multi-Year Drought (MYD) events,
#' where various soil desiccation thresholds could not be reached due to insufficient rainfall for soil moisture recovery to field capacity.
#' For a detailed explanation of the methodologies applied to retrieve each missing drought feature by threshold or soil desiccation levels, users should consult the work of Elias et al. (2024).
#' Some years, even after MYD recalculation, may still have missing values for extreme drought onset, offset, duration, and severity. This primarily occurs when the KBDI time series desiccation for those years does not exceed the KBDI-150 threshold.
#' @param kbdi_data A data frame containing daily KBDI data, which is the output of the `calculate_KBDI` function.
#'  It is crucial to ensure that all years are fully present, in chronological order, and without any missing KBDI values for any given day.
#' @param minDmax A data frame containing the days since beginning of minimum kbdi value between two consecutive peak drought days and its corresponding KBDI value (min_KBDI), which is the output of the `identify_min_kbdi_between_two_KBDI_peaks` function
#' @drought_features_data the extracted drought features tables with potential missing features, which is the output of the `Drought_features_extraction` function
#' @param save_to_csv A logical value (`TRUE` or `FALSE`). If `TRUE`, the function will save the results to a CSV file. Default is `FALSE`.
#' @param file_name A string specifying the name of the CSV file to save the results if `save_to_csv` is `TRUE`. Default is "DF_characterization_under_MYD.csv".
#' @return The function returns the same drought features table, but with missing values recalculated for each hydrological year.
#' @examples
#' # calculate KBDI
#' result_kbdi <- calculate_KBDI(tab_clim)
#'
#' # calculate Peak.KBDI.DOY and its corresponding value Peak.KBDI over each hydrological year
#' peak_drought_info <- identify_peak_drought_days(result_kbdi)
#'
#' # Identify Minimum KBDI day and corresponding value Between Two Consecutive Peak Drought Days (Peak.KBDI.DOY)
#' min_kbdi_between_drought_peaks <- identify_min_kbdi_between_two_drought_peaks(result_kbdi, peak_drought_info)
#'
#' # Initialize an empty table to store the extracted drought features
#' drought_features_table  <- initialize_drought_features_table(result_kbdi)
#'
#'
#' # Drought features extraction
#' DF <- Drought_features_extraction (kbdi_data = result_kbdi,
#'                                    minDmax = min_kbdi_between_drought_peaks,
#'                                    drought_features_table = drought_features_table,
#'                                    save_to_csv = F,
#'                                    plot_features_extraction = T)
#'
#' # Multi-year drought characterization
#' MYD_features_calculation <- MYD_characterization(kbdi_data = result_kbdi,
#'                                                  minDmax = min_kbdi_between_drought_peaks,
#'                                                  drought_features_data = DF,
#'                                                  save_to_csv = FALSE)
#'


MYD_characterization <- function (kbdi_data, minDmax, drought_features_data, save_to_csv = FALSE, file_name = 'DF_characterization_under_MYD.csv') {

  # Ensure the necessary columns are in the kbdi_data
  if (!all(c("year", "month", "day", "KBDI") %in% colnames(kbdi_data))) {
    stop("The KBDI data must contain the following columns: 'year', 'month', 'day', and 'KBDI'")
  }

  # Ensure that the necessary columns exist in minDmax
  if (!all(c("minDmax_DOY", "min_KBDI") %in% colnames(minDmax))) {
    stop("Required columns 'minDmax_DOY' and 'min_KBDI' must be present in the minDmax data frame")
  }

  # Calculate the number of years
  number_of_years <- floor(nrow(kbdi_data) / 365.25)

  # Check if the number of rows in the drought_features_table matches the number of years
  if (nrow(drought_features_table) != number_of_years-1 ) {
    stop("The number of rows in the drought_features_table does not match the number of years. Please check the data.")
  }

  # Determine the start year from the first entry in the 'year' column of the KBDI data
  start_year <- kbdi_data$year[1]

  # Calculate first January of each hydrological year in Day Since Beginning (DSB) which will help[ in next steps calculation
  # example : 1 DSB = 1st January 1960, while 366 DSB = 1st January 1961...
  Jan_first_SB <- seq(1,(number_of_years ), by = 1)

  for (year in 1: (number_of_years))  {
    Jan_first_SB[year] <- trunc(1 + (year - 1)*365.25)
  }


  for (year in 1:(number_of_years - 1))
  {

    print(c( 'We are removing NA in the year', start_year + (year - 1) ))

    # Re-calculate Low Drought Offset

    n= year
    t1_t <- 0
    while (is.na(drought_features_data$Low.D.Offset[n])) {
      if (n == 1) {
        start_calc <- Jan_first_SB[n]
        end_calc <- Jan_first_SB[n + 1]
      } else if ( (n > 1) & !is.na(drought_features_data$Low.D.Onset[n])) {
        start_calc <- Jan_first_SB[n]
        end_calc <- Jan_first_SB[n + 1]
      } else if ( (n > 1) & is.na(drought_features_data$Low.D.Onset[n]) & is.na(drought_features_data$Low.D.Onset[n + 1])) {
        start_calc <- Jan_first_SB[n]
        end_calc <- Jan_first_SB[n + 1]
      }


      t1_t = length(seq(start_calc,end_calc, by = 1)) + t1_t
      n = n + 1
      if (n == number_of_years) {
        break }

    }

    if (is.na( drought_features_data$Low.D.Offset[year]) & !is.na(drought_features_data$Low.D.Offset[n])) {
      drought_features_data$Low.D.Offset[year]  <- t1_t + drought_features_data$Low.D.Offset[n]
    }  else if (is.na( drought_features_data$Low.D.Offset[year]) & is.na(drought_features_data$Low.D.Offset[dim(drought_features_data)[1]])) {
      drought_features_data$Low.D.Offset[year]  <- t1_t
    }



    # Re-calculate Low Drought Onset

    if (year == 1) {
      drought_features_data$Low.D.Onset[year] <- drought_features_data$Low.D.Onset[year]
    }

    # Delayed Drought Onset Calculation of the second  drought year
    # based on the low drought onset of the first year [year-1]
    #
    if ( (year == 2) & is.na(drought_features_data$Low.D.Onset[year])) {
      drought_features_data$Low.D.Onset[year] <- (length(seq(drought_features_data$Low.D.Onset[year - 1], Jan_first_SB[year], by = 1)))*-1
    }
    if (year == 1) {
      drought_features_data$Low.D.Onset[year] <- drought_features_data$Low.D.Onset[year]
    }  else if ( (year > 2) & is.na(drought_features_data$Low.D.Onset[year]) & drought_features_data$Low.D.Onset[year - 1] > 0) {

      var <- length (seq(1, Jan_first_SB[year - 1])) + drought_features_data$Low.D.Onset[year - 1]
      drought_features_data$Low.D.Onset[year] <- (Jan_first_SB[year] - var) * -1
    }  else if ( (year > 2) & is.na(drought_features_data$Low.D.Onset[year]) & drought_features_data$Low.D.Onset[year - 1] < 0)  {

      drought_features_data$Low.D.Onset[year] <- ( abs(drought_features_data$Low.D.Onset[year - 1]) + 366) * -1
    }


    # Re-calculate Low Drought Duration and Severity

    if (is.na(drought_features_data$Low.D.S[year]) & is.na(drought_features_data$Low.D.S[year + 1]) & (year == 1)) {
      drought_features_data$Low.D.S[year] <- sum(kbdi_data$KBDI[ drought_features_data$Low.D.Onset[year] :minDmax$minDmax_DOY[year]])
      drought_features_data$Low.D.Duration[year] <- length(seq(drought_features_data$Low.D.Onset[year], minDmax$minDmax_DOY[year], by = 1 ))

    } else if (is.na(drought_features_data$Low.D.S[year]) & !is.na(drought_features_data$Low.D.S[year + 1]) & (year > 1) & (drought_features_data$Low.D.Onset[year] < 0) ) {

      end_date_1 <- length (seq (1, Jan_first_SB[year], by = 1)) + drought_features_data$Low.D.Offset[year]
      drought_features_data$Low.D.S[year] <- drought_features_data$Low.D.S[year - 1] + sum(kbdi_data$KBDI[minDmax$minDmax_DOY[year - 1]: end_date_1])
      drought_features_data$Low.D.Duration[year] <- drought_features_data$Low.D.Duration[year - 1] + length(seq(minDmax$minDmax_DOY[year - 1], end_date_1, by = 1))

    }  else if ( is.na(drought_features_data$Low.D.S[year]) & is.na(drought_features_data$Low.D.S[year + 1]) & (year > 1) & (drought_features_data$Low.D.Onset[year] < 0) & !is.na (drought_features_data$Low.D.Onset[year + 1])) {
      end_date_1 <- length (seq (1, Jan_first_SB[year], by = 1)) + drought_features_data$Low.D.Offset[year]
      drought_features_data$Low.D.S[year] <- drought_features_data$Low.D.S[year - 1] + sum(kbdi_data$KBDI[minDmax$minDmax_DOY[year - 1]: end_date_1])
      drought_features_data$Low.D.Duration[year] <- drought_features_data$Low.D.Duration[year - 1] + length(seq(minDmax$minDmax_DOY[year - 1], end_date_1, by = 1))

    } else if (is.na(drought_features_data$Low.D.S[year]) & (year > 1) & (drought_features_data$Low.D.Onset[year] > 0) & is.na(drought_features_data$Low.D.S[year + 1])) {
      start_date <- length(seq(1, Jan_first_SB[year], by = 1)) + drought_features_data$Low.D.Onset[year]
      drought_features_data$Low.D.S[year] <- sum(kbdi_data$KBDI[start_date: minDmax$minDmax_DOY[year]])
      drought_features_data$Low.D.Duration[year] <- length(seq(start_date, minDmax$minDmax_DOY[year], by = 1))

    } else if (is.na(drought_features_data$Low.D.S[year]) & is.na(drought_features_data$Low.D.S[year + 1]) & (drought_features_data$Low.D.Onset[year] < 0) & is.na(drought_features_data$Low.D.Onset[year + 1]) ) {
      drought_features_data$Low.D.S[year] <- drought_features_data$Low.D.S[year - 1] + sum(kbdi_data$KBDI[minDmax$minDmax_DOY[year-1]:minDmax$minDmax_DOY[year]])
      drought_features_data$Low.D.Duration[year] <- drought_features_data$Low.D.Duration[year - 1] + length(seq(minDmax$minDmax_DOY[year-1], minDmax$minDmax_DOY[year], by = 1))

    } else if (is.na(drought_features_data$Low.D.S[year]) & (year == number_of_years - 1 ) & (drought_features_data$Low.D.Onset[number_of_years - 2] < 0)) {
      drought_features_data$Low.D.S[year] <- drought_features_data$Low.D.S[year - 1] + sum(kbdi_data$KBDI[minDmax$minDmax_DOY[year - 1]:minDmax$minDmax_DOY[year]])
      drought_features_data$Low.D.Duration[year] <- drought_features_data$Low.D.Duration[year - 1] + length(seq(minDmax$minDmax_DOY[year - 1], minDmax$minDmax_DOY[year], by = 1))

    }

    # Re-calculate Moderate Drought Offset

    q = year
    t1m_t <- 0
    while (is.na( drought_features_data$Mod.D.Offset[q])) {
      if (q == 1) {
        start_calc <- Jan_first_SB[q]
        end_calc <- Jan_first_SB[q + 1]
      } else if ( (q > 1) & !is.na(drought_features_data$Mod.D.Onset[q]) ) {
        start_calc <- Jan_first_SB[q]
        end_calc <- Jan_first_SB[q + 1]
      } else if ( (q > 1) & is.na(drought_features_data$Mod.D.Onset[q]) & is.na(drought_features_data$Mod.D.Onset[q + 1])) {
        start_calc <- Jan_first_SB[q]
        end_calc <- Jan_first_SB[q + 1]
      }

      t1m_t = length(seq(start_calc, end_calc, by = 1)) + t1m_t
      q = q + 1
      if (q == number_of_years) {
        break
      }

    }

    if (is.na( drought_features_data$Mod.D.Offset[year]) & !is.na(drought_features_data$Mod.D.Offset[q])) {
      drought_features_data$Mod.D.Offset[year]  <- t1m_t + drought_features_data$Mod.D.Offset[q]
    }  else if ( is.na( drought_features_data$Mod.D.Offset[year]) & is.na( drought_features_data$Mod.D.Offset[dim(drought_features_data)[1]])) {
      drought_features_data$Mod.D.Offset[year]  <- t1m_t
    }

    # Re-Calculate Moderate Drought Onset

    if (year == 1) {
      drought_features_data$Mod.D.Onset[year] <- drought_features_data$Mod.D.Onset[year]
    }


    if ( (year == 2) & is.na(drought_features_data$Mod.D.Onset[year])) {
      drought_features_data$Mod.D.Onset[year] <- (length(seq(drought_features_data$Mod.D.Onset[year - 1], minDmax$minDmax_DOY[1], by = 1)))* -1
    }

    if (year == 1) {
      drought_features_data$Mod.D.Onset[year] <- drought_features_data$Mod.D.Onset[year]
    }  else if ( (year > 2) & is.na(drought_features_data$Mod.D.Onset[year]) & (drought_features_data$Mod.D.Onset[year - 1] > 0)) {
      var <- length (seq(1, Jan_first_SB[year - 1])) +  drought_features_data$Mod.D.Onset[year - 1]

      drought_features_data$Mod.D.Onset[year] <- (Jan_first_SB[year] - var) * -1
    } else if ( (year > 2) & is.na(drought_features_data$Mod.D.Onset[year]) & (drought_features_data$Mod.D.Onset[year-1] < 0))  {
      drought_features_data$Mod.D.Onset[year] <- (abs(drought_features_data$Mod.D.Onset[year - 1]) + 366) * -1
    }


    # Re-calculate Moderate Drought Duration and Severity

    if (is.na(drought_features_data$Mod.D.S[year]) & is.na(drought_features_data$Mod.D.S[year + 1]) & (year == 1)) {
      drought_features_data$Mod.D.S[year] <- sum(kbdi_data$KBDI[drought_features_data$Mod.D.Onset[year]:minDmax$minDmax_DOY[year]])
      drought_features_data$Mod.D.Duration[year] <- length(seq(drought_features_data$Mod.D.Onset[year], minDmax$minDmax_DOY[year], by = 1))

    } else if (is.na(drought_features_data$Mod.D.S[year]) & !is.na(drought_features_data$Mod.D.S[year + 1]) & (year > 1) & (drought_features_data$Mod.D.Onset[year] < 0) ){

      end_date_1 <- length (seq (1, Jan_first_SB[year], by = 1)) + drought_features_data$Mod.D.Offset[year]
      drought_features_data$Mod.D.S[year] <- drought_features_data$Mod.D.S[year - 1] + sum(kbdi_data$KBDI[minDmax$minDmax_DOY[year - 1]:end_date_1])
      drought_features_data$Mod.D.Duration[year] <- drought_features_data$Mod.D.Duration[year - 1] + length(seq(minDmax$minDmax_DOY[year - 1], end_date_1, by = 1))

    }  else if ( is.na( drought_features_data$Mod.D.S[year]) & is.na( drought_features_data$Mod.D.S[year + 1]) & (year > 1) & (drought_features_data$Mod.D.Onset[year] < 0) & !is.na (drought_features_data$Mod.D.Onset[year + 1])) {
      end_date_1 <- length (seq (1, Jan_first_SB[year], by = 1)) + drought_features_data$Mod.D.Offset[year]
      drought_features_data$Mod.D.S[year] <- drought_features_data$Mod.D.S[year - 1] + sum(kbdi_data$KBDI[minDmax$minDmax_DOY[year - 1]:end_date_1])
      drought_features_data$Mod.D.Duration[year] <- drought_features_data$Mod.D.Duration[year - 1] + length(seq(minDmax$minDmax_DOY[year - 1], end_date_1, by = 1))

    } else if (is.na(drought_features_data$Mod.D.S[year]) & (year > 1) & (drought_features_data$Mod.D.Onset[year] > 0) & is.na(drought_features_data$Mod.D.S[year + 1])) {
      start_date <- length(seq(1, Jan_first_SB[year], by = 1)) + drought_features_data$Mod.D.Onset[year]
      drought_features_data$Mod.D.S[year] <- sum(kbdi_data$KBDI[start_date: minDmax$minDmax_DOY[year]])
      drought_features_data$Mod.D.Duration[year] <- length(seq(start_date, minDmax$minDmax_DOY[year], by = 1))

    } else if (is.na(drought_features_data$Mod.D.S[year]) & is.na(drought_features_data$Mod.D.S[year + 1]) & (drought_features_data$Mod.D.Onset[year] < 0) & is.na(drought_features_data$Mod.D.Onset[year + 1]) ) {
      drought_features_data$Mod.D.S[year] <- drought_features_data$Mod.D.S[year - 1] + sum(kbdi_data$KBDI[minDmax$minDmax_DOY[year-1]:minDmax$minDmax_DOY[year]])
      drought_features_data$Mod.D.Duration[year] <- drought_features_data$Mod.D.Duration[year-1] + length(seq(minDmax$minDmax_DOY[year-1], minDmax$minDmax_DOY[year], by = 1))

    } else if (is.na(drought_features_data$Mod.D.S[year]) & (year == number_of_years-1) & (drought_features_data$Mod.D.Onset[number_of_years-2] < 0)) {
      drought_features_data$Mod.D.S[year] <- drought_features_data$Mod.D.S[year - 1] + sum(kbdi_data$KBDI[minDmax$minDmax_DOY[year - 1]:minDmax$minDmax_DOY[year]])
      drought_features_data$Mod.D.Duration[year] <- drought_features_data$Mod.D.Duration[year - 1] + length(seq(minDmax$minDmax_DOY[year - 1], minDmax$minDmax_DOY[year], by = 1))

    }

    print(c('End of the iteration for the year',start_year + (year - 1) ))

    #
    # end
  }



  return(drought_features_data)


  # If save_to_csv is TRUE, save the result as a CSV file
  if (save_to_csv) {
    write.csv(drought_features_data, file = file_name, row.names = FALSE)
    message(paste("File saved as", file_name))
  }

}
