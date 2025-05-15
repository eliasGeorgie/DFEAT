
#' @title Yearly Drought Features Extraction
#' @description This function extracts 19 annual drought features for each hydrological year, starting on January 1st
#' and ending either when soil moisture is replenished to field capacity (as indicated by a minimum
#' KBDI near 0 between two peak drought days) or, if full replenishment is not reached, on the corresponding minDmax_DOY. Drought features extraction are based on three different soil desiccation thresholds,
#' where soil losses 25%, 50%, and 75% of its total available water content. The extracted features for
#' each year are saved in the pre-initialized table created by the `initialize_drought_features_table`function.
#' Detailed descriptions of each drought feature can be found in the foundational work by Elias et al., 2024.
#' Due to the incomplete return of soil moisture to field capacity at the end of some dry seasons under semi-arid and arid climatic conditions, Multi-Year Drought (MYD) events
#' can extend over consecutive hydrological years, possibly affecting various soil desiccation thresholds.
#' As a result, the output of this function may still contain missing values (NA) for some drought features that could not be calculated within those hydrological years.
#' Users should subsequently apply the MYD_characterization function to re-calculate and complete these missing features in the context of prolonged MYD events.
#' @param kbdi_data A data frame containing daily 'KBDI' data, and 'year', 'month', and 'day' columns, which are the output of the `calculate_KBDI` function.
#' It is crucial to ensure that all years are fully present, in chronological order, and without any missing KBDI values for any given day.
#' @param minDmax A data frame containing minDmax_DOY (in day since beginning) and its corresponding KBDI value (min_KBDI), which is the output of the `identify_min_kbdi_between_two_drought_peaks` function
#' @param drought_features_table an empty table for storing extracted drought features, which is the output of the `initialize_drought_features_table` function
#'  As previously described, this function will exclude the last year in the provided climate dataset as we cannot characterize it due to lack of sufficient KBDI data on its subsequent year.
#' @param save_to_csv A logical value (`TRUE` or `FALSE`). If `TRUE`, the function will save the results to a CSV file. Default is `FALSE`.
#' @param plot_features_extraction A logical value (`TRUE` or `FALSE`). If `TRUE`, the function will plots the results of drought features extraction. Default is `TRUE`.
#' @param file_name A string specifying the name of the CSV file to save the results if `save_to_csv` is `TRUE`. Default is "Drought_Features_table.csv".
#' @return The function returns a data frame populated with the extracted drought features for each hydrological year.
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
#'  head(DF)


Drought_features_extraction <- function(kbdi_data, minDmax, drought_features_table, save_to_csv = FALSE, plot_features_extraction = TRUE, file_name = 'Drought_Features_table.csv') {

  # Ensure the necessary columns are in the kbdi_data
  if (!all(c("year", "month", "day", "KBDI") %in% colnames(kbdi_data))) {
    stop("The KBDI data must contain the following columns: 'year', 'month', 'day', and 'KBDI'")
  }

  # Ensure that the necessary columns exist in minDmax
  if (!all(c("minDmax_DOY", "min_KBDI") %in% colnames(minDmax))) {
    stop("Required columns 'minDmax_DOY' and 'min_KBDI' must be present in the minDmax data frame")
  }

  # Calculate the number of years in the dataset and exclude the last year as we cannot characterize it due to lack of sufficient KBDI data on its subsequent year
  number_of_years <- length(unique(kbdi_data$year)) - 1

  # Check if the number of rows in the drought_features_table matches the number of years
  if (nrow(drought_features_table) != number_of_years) {
    stop("The number of rows in the drought_features_table does not match the number of years. Please check the data.")
  }

  # Determine the start year from the first entry in the 'year' column of kbdi_data
  start_year <- kbdi_data$year[1]

  # thresholds to identify Low/Moderate/Extreme drought onset
  KBDI_50_onset <- 50
  KBDI_100_onset <- 100
  KBDI_150_onset <- 150

  # thresholds to identify Low/Moderate/Extreme drought offset
  KBDI_50_offset <- 50
  KBDI_100_offset  <- 100
  KBDI_150_offset <- 150

  # Field capacity in mm
  FC <- 200

  # for fittage correction purpose
  low_offset_hydro_year <- 365

  # start of Drought Feature Extraction over each hydrological year since the first year in the KBDI time serie table (kbdi_data)
  for (year in 1:number_of_years)
  {

    print(c('we are fitting the year', start_year + (year - 1)))

    # t0 is the start day of each hydrological year since beginning (e.g., 1st January, 1960; 1st January, 1961; 1st January, 1962... )
    t0 <- trunc(1 + (year - 1) * 365.25)

    # end_hydro_year is  the end of each hydrological year since beginning,
    # presented by the first day the soil reach its soil capacity after summer drought
    # or if there are no full return to field capacity, the end_hydro_year will be the minimum
    # index value between two consecutive hydrological year peak drought days
    end_hydro_year <- minDmax$minDmax_DOY[year]

    # t is a sequence used for fitting purpose
    t <- seq(1, minDmax$minDmax_DOY[year], 1)

    # corrections for hydrological year's which do not reach the soil's field capacity
    # with different in KBDI value between onset and offset, the fittage quality (r-squared) is reduced
    if (year == 1) {
      t1 <- end_hydro_year
    } else if ((year > 1) & (minDmax$min_KBDI[year - 1] - minDmax$min_KBDI[year] > 40)) {
      t1 <- which(kbdi_data$KBDI[t0:end_hydro_year] < minDmax$min_KBDI[year - 1])[1] + trunc((year - 1) * 365.25)
    } else if (year > 1) {
      t1 <- end_hydro_year
    }


    # "tab_moisture_year" present the KBDI values from the start day (t0) to the end day's (t1) of each hydrological year
    tab_moisture_year <- kbdi_data$KBDI[t0:t1]

    # corrections for the start of each hydrological year
    # in the case if the KBDI is still high and not reach 0 after 31 December

    if ((year > 1) & (low_offset_hydro_year > 365)) {
      tab_moisture_year[1:(minDmax$minDmax_DOY[year - 1] - trunc((year - 1) * 365.25))] <- tab_moisture_year[minDmax$minDmax_DOY[year - 1] - trunc((year - 1) * 365.25) + 1]
    }
    if ((year > 1) & (low_offset_hydro_year < 365)) {
      tab_moisture_year[1:20] <- minDmax$min_KBDI[year - 1]
    }

    # Drought peak intensity (D_max) and its corresponding day of the year (D_max_DOY)
    D_max <- max(tab_moisture_year)
    D_max_DOY <- which(tab_moisture_year[] == D_max)

    # our retained Fine curve fitting method (M) (Elmore et al. 2012)
    M <- "Elmore"

    # fit_period is the total number of days of each hydrological year
    fit_period = length(tab_moisture_year)

    # fine curve fitting based on the length of tab_moisture_year
    fit <- phenofit::curvefit(tab_moisture_year, seq(1, fit_period, 1), seq(1, fit_period, 1), methods = M )

    # get yearly vegetation phenological metrics of the fitted curve
    l_pheno <- phenofit::get_pheno(fit, M, IsPlot = FALSE)

    # extract the 7 parameters of Elmore Equation
    mn <- fit$model$Elmore$par[1]
    mx <- fit$model$Elmore$par[2]
    sos <- fit$model$Elmore$par[3]
    rsp <- fit$model$Elmore$par[4]
    eos <- fit$model$Elmore$par[5]
    rau <- fit$model$Elmore$par[6]
    m7 <- fit$model$Elmore$par[7]

    # fitted lines (Elmore Equation)
    elmore <- mn + (mx - m7 * t) * (1 / (1 + exp(-rsp * (t - sos))) - 1 / (1 + exp(-rau * (t - eos))))

    E_greendown_parameter <- mn + (mx - m7 * t)

    # first exponential line --> drying_curve : represent Elmore Increasing curve or Soil Moisture Depletion for us
    drying_curve <- mn +(mx - m7 * t) * (1 / (1 + exp(-rsp * (t - sos))))

    # second exponential line --> wetting_curve: represent Elmore Decreasing curve or soil Moisture Replenishement/Recovery for us
    wetting_curve <- mn + (mx - m7 * t) * (1 - 1 / (1 + exp(-rau * (t - eos))))

    # Mathematical derivation of the wetting_curve
    wetting_curve_der <- (-m7) - (-m7 * (1 + exp(-rau * (t - eos))) - (mx - m7 * t) * (-rau) * exp(-rau * (t - eos))) / (1 + exp(-rau * (t - eos)))^2

    # Mathematical derivation of the drying_curve
    drying_curve_der <-  ( -m7) *(1/(1 + exp(-rsp * (t - sos)))) + (mx - m7 * t) * (rsp * exp(-rsp * (t - sos)) / (1 + exp(-rsp * (t - sos)))^2)


    # Determine three level of Drought Onset based on drying_curve_d
    # in order the Determine the "Drying Rate" feature

    mod_onset_from_der <- which(drying_curve_der[] == max(drying_curve_der[], na.rm = TRUE))
    drying_rate <- drying_curve_der[mod_onset_from_der]
    intercept_drying_curve <- drying_curve[mod_onset_from_der] - drying_curve_der[mod_onset_from_der] * mod_onset_from_der
    low_onset_from_der <- (-intercept_drying_curve) / drying_rate
    extreme_onset_from_der <- trunc((mn + mx - intercept_drying_curve) / ( drying_rate + m7))

    # Determine three level of Drought Offset based on wetting_curve_d
    # in order to Determine the "Wetting Rate" feature

    mod_offset_from_der <- which(wetting_curve_der[] == min(wetting_curve_der[], na.rm = TRUE))
    wetting_rate <- wetting_curve_der[mod_offset_from_der]
    intercept_wetting_rate <- wetting_curve[mod_offset_from_der] - wetting_curve_der[mod_offset_from_der] * mod_offset_from_der
    low_offset_from_der <- (-intercept_wetting_rate) / wetting_rate
    extreme_offset_from_der <- trunc((mn + mx - intercept_wetting_rate) / (wetting_rate + m7))


    # Determination of  three Drought Onset for the three retained soil desiccation thresholds

    # Determination of Low Drought Onset presented by "low_onset"

    nb_obs <- length(elmore[1:(minDmax$minDmax_DOY[year] - trunc((year - 1) * 365.25))])

    if (elmore[1] < KBDI_50_onset) {

      low_onsets_in_fitperiod <- which(elmore[1:D_max_DOY] > KBDI_50_onset)
      low_onset <- low_onsets_in_fitperiod[1]
    } else if (elmore[1] > KBDI_50_onset) {
      low_onset <- NA
    }

    if (year == 1) {
      low_onset <- low_onset
    } else if ((year > 1) & !is.na(low_onset) & (minDmax$min_KBDI[year - 1] > KBDI_50_onset)) {
      low_onset <- NA
    }


    # Determination of Moderate Drought Onset presented by "mod_onset"

    if (elmore[1] < KBDI_100_onset){

      mod_onsets_in_fitperiod <- which(elmore[1:D_max_DOY] > KBDI_100_onset)
      mod_onset <-  mod_onsets_in_fitperiod[1]
    } else if (elmore[1] > KBDI_100_onset) {
      mod_onset <- NA
    }

    if (year == 1) {
      mod_onset <-  mod_onset
    } else if ((minDmax$min_KBDI[year-1] > KBDI_100_onset) & !is.na(mod_onset)) {
      mod_onset <- NA
    }

    # Determination of Extreme Drought Onset presented by "extreme_onset"

    if (max(elmore[1:(minDmax$minDmax_DOY[year] - trunc((year - 1) * 365.25))]) > KBDI_150_onset ) {
      extreme_onsets_in_fitperiod <-  which(elmore[1:D_max_DOY] > KBDI_150_onset)
      extreme_onset <- extreme_onsets_in_fitperiod[1]
    } else if (max(elmore[1:(minDmax$minDmax_DOY[year] -trunc((year - 1) * 365.25))]) < KBDI_150_onset ) {
      extreme_onset <- NA
    }

    if ( !is.na(extreme_onset) &  (extreme_onset== 1)) {
      Extreme_Onset <- which(tab_moisture_year[1:D_max_DOY] > KBDI_150_onset)
      extreme_onset <- Extreme_Onset[1]
    }


    # Determination of the three Drought Offset for the three retained soil desiccation thresholds

    # Determination of Extreme Drought Offset presented by "extreme_offset"

    if (is.na(extreme_onset)) {
      extreme_offset <- NA
    } else if (max(elmore[1:(minDmax$minDmax_DOY[year] - trunc((year - 1) * 365.25))]) > KBDI_150_offset) {
      extreme_offsets_in_fitperiod <- which(elmore[D_max_DOY:(minDmax$minDmax_DOY[year] - trunc((year - 1) * 365.25))] < KBDI_150_offset)
      extreme_offset <- extreme_offsets_in_fitperiod[1] + length(elmore[1:D_max_DOY])
    }

    # Determination of Moderate Drought Offset presented by "mod_offset"

    if (elmore[nb_obs] > KBDI_100_offset ) {
      mod_offset <- NA
    } else if (elmore[nb_obs] < KBDI_100_offset) {
      mod_offsets_in_fitperiod <- which(elmore[D_max_DOY:(minDmax$minDmax_DOY[year] -trunc((year - 1) * 365.25))] < KBDI_100_offset)
      mod_offset <- mod_offsets_in_fitperiod[1] + length(elmore[1:D_max_DOY])
    }

    if ((minDmax$min_KBDI[year] > KBDI_100_offset) & !is.na(mod_offset)) {
      mod_offset <- NA
    }

    # Determination of Low Drought Offset presented by "low_offset"

    if (elmore[nb_obs] > KBDI_50_offset ) {
      low_offset <- NA
    } else if (elmore[nb_obs] < KBDI_50_offset ) {
      low_offsets_in_fitperiod <- which(elmore[D_max_DOY:((minDmax$minDmax_DOY[year] - trunc((year - 1) * 365.25)) + 20)] < KBDI_50_offset)
      low_offset <- low_offsets_in_fitperiod[1] +  length(elmore[1:D_max_DOY])
    }

    if ((minDmax$min_KBDI[year]> KBDI_50_offset) & !is.na(low_offset)) {
      low_offset <- NA
    }


    # Retrieve low drought offset (low_offset) in case where  KBDI time serie is cutted for fitting purpose

    if ((year > 1) & is.na(low_offset) & (minDmax$min_KBDI[year] < KBDI_50_offset)) {
      new_tabmoisture <- kbdi_data$KBDI[t0:end_hydro_year]
      if ((year > 1) & (low_offset_hydro_year > 365)) {
        new_tabmoisture[1:(minDmax$minDmax_DOY[year - 1] - trunc((year - 1) * 365.25)) ]<- new_tabmoisture[minDmax$minDmax_DOY[year - 1] - trunc((year - 1) * 365.25) + 1 ]
      }
      if ((year > 1) & (low_offset_hydro_year < 365)) {
        new_tabmoisture[1:20]<- minDmax$min_KBDI[year - 1]
      }

      fitperiod_original = length(new_tabmoisture)

      fit1 <- phenofit::curvefit(new_tabmoisture, seq(1, fitperiod_original, 1), seq(1, fitperiod_original, 1), methods = M )
      mn1 <- fit1$model$Elmore$par[1]
      mx1 <- fit1$model$Elmore$par[2]
      sos1 <- fit1$model$Elmore$par[3]
      rsp1 <- fit1$model$Elmore$par[4]
      eos1 <- fit1$model$Elmore$par[5]
      rau1 <- fit1$model$Elmore$par[6]
      m71 <- fit1$model$Elmore$par[7]

      elmore_before_cutting <- mn1 + (mx1 - m71 * t) * (1 / (1 + exp(-rsp1 * (t - sos1))) - 1 / (1 + exp(-rau1 * (t - eos1))))
      max_elmore_before_cutting <- max(elmore_before_cutting[1:((minDmax$minDmax_DOY[year] - trunc((year - 1) * 365.25)))])

      low_offset <- which(elmore_before_cutting[D_max_DOY:((minDmax$minDmax_DOY[year] - trunc((year - 1) * 365.25)) + 50)] < KBDI_50_offset)[1] + length(max_elmore_before_cutting[1:D_max_DOY])
    }

    if ((year > 1) & is.na(low_offset) & ( minDmax$min_KBDI[year] < KBDI_50_offset))
    {
      offset <- which(tab_moisture_year[D_max_DOY:trunc((year - 1) * 365.25)] < KBDI_50_offset)
      low_offset <- offset[1] + length(tab_moisture_year[1:D_max_DOY])
    }

    if ((year == 1) & !is.na(low_offset) &  minDmax$min_KBDI[year] > KBDI_50_offset ) {
      low_offset <- NA
    } else if ((year > 1) & !is.na(low_offset) &  minDmax$min_KBDI[year] > KBDI_50_offset) {
      low_offset <- NA
    }

    # Retrieve Moderate Drought Offset (mod_offset) in case where  KBDI time serie is cutted for fitting purpose


    if ((year > 1) & is.na(mod_offset ) & ( minDmax$min_KBDI[year] < KBDI_100_offset))
    {
      new_tabmoisture <- kbdi_data$KBDI[t0:end_hydro_year]
      if ((year > 1) & (low_offset_hydro_year > 365)) {
        new_tabmoisture[1:(minDmax$minDmax_DOY[year - 1] - trunc((year - 1) * 365.25))] <- new_tabmoisture[minDmax$minDmax_DOY[year - 1] - trunc((year - 1) * 365.25) + 1 ]
      }
      if ((year > 1) & (low_offset_hydro_year < 365)) {
        new_tabmoisture[1:20]<- minDmax$min_KBDI[year-1]
      }

      fitperiod_original=length(new_tabmoisture)
      fit2<- phenofit::curvefit(new_tabmoisture, seq(1,fitperiod_original,1), seq(1,fitperiod_original,1), methods= M )
      mn2<-fit2$model$Elmore$par[1]
      mx2<-fit2$model$Elmore$par[2]
      sos2<-fit2$model$Elmore$par[3]
      rsp2<-fit2$model$Elmore$par[4]
      eos2<-fit2$model$Elmore$par[5]
      rau2<-fit2$model$Elmore$par[6]
      m72<-fit2$model$Elmore$par[7]

      elmore_before_cutting <-mn2 + (mx2 - m72 * t) * (1 / (1 + exp(-rsp2 * (t - sos2))) - 1/(1 + exp(-rau2 * (t - eos2))))
      max_elmore_before_cutting <- max(elmore_before_cutting[1:((minDmax$minDmax_DOY[year] - trunc((year - 1) * 365.25)) + 50)])

      mod_offset <- which(elmore_before_cutting[D_max_DOY:((minDmax$minDmax_DOY[year] - trunc((year - 1) * 365.25)) + 50)] < KBDI_100_offset )[1] + length(max_elmore_before_cutting[1:D_max_DOY])
    }

    if ((year >= 1) & !is.na(mod_offset) & minDmax$min_KBDI[year] > KBDI_100_offset ) {
      mod_offset <- NA
    }


    # Determination of  three Drought Duration for the three retained soil desiccation thresholds

    if (is.na(low_offset) | is.na(low_onset)) {
      KBDI_DD_low_50 <- NA
    } else {
      KBDI_DD_low_50 <- low_offset - low_onset
    }
    if (is.na(mod_offset) | is.na(mod_onset)) {
      KBDI_DD_mod_100 <- NA
    } else {
      KBDI_DD_mod_100 <- mod_offset - mod_onset
    }
    if (is.na(extreme_offset) | is.na(extreme_onset)) {
      KBDI_DD_extreme_150 <-0
    } else {
      KBDI_DD_extreme_150 <- extreme_offset - extreme_onset
    }

    # this low offset is based on elmore phenological phases extraction
    low_offset_hydro_year <- unname(l_pheno$TRS2[2])

    if (plot_features_extraction) {
      # plot  KBDI over the considered hydrological year (between t0 and t1)
      plot(tab_moisture_year, type='l', ylab = 'KBDI', xlab =  c('DOY since 1st January', start_year + (year - 1)), main = start_year + (year - 1), xlim = c(1, 500), bty = 'n')
    }

    # adjust the end of each hydrological year in order to complete the Drought Facets Calculation
    t1 <- trunc(low_offset_from_der + ((year - 1) * 365.25) + 20)

    tab_moisture_year <- kbdi_data$KBDI[t0:t1]

    # Determination of  three Drought Severity for the three retained soil desiccation thresholds

    if (is.na(low_offset) | is.na(low_onset)){
      KBDI_DS_low_50 <- NA
    } else {
      KBDI_DS_low_50 <- sum(tab_moisture_year[low_onset:low_offset], na.rm = T)
    }
    if (is.na(mod_offset) | is.na(mod_onset)) {
      KBDI_DS_mod_100 <- NA
    } else {
      KBDI_DS_mod_100 <- sum(tab_moisture_year[mod_onset:mod_offset], na.rm = T)
    }

    if (is.na(extreme_offset) | is.na(extreme_onset)) {
      KBDI_DS_extreme_150 <- 0
    } else {
      KBDI_DS_extreme_150 <- sum(tab_moisture_year[extreme_onset:extreme_offset], na.rm = T)
    }

    # reverse the KBDI value to Calculate Rainfall Pulses
    reversed_tab_moisture <- FC - tab_moisture_year


    # Rainfall-pulses number and intensity calculation (in mm)

    if (is.na(extreme_offset)) {
      RPN <- 0
      RPImean <- 0
      RPIsd <- 0
    } else if (!is.na(extreme_offset)) {

      diff0 <- (reversed_tab_moisture[D_max_DOY:extreme_offset] - reversed_tab_moisture[(D_max_DOY - 1):(extreme_offset -1 )])
      diff1 <- (reversed_tab_moisture[(D_max_DOY + 1):(extreme_offset + 1)] - reversed_tab_moisture[(D_max_DOY):(extreme_offset )])
      if((sum(diff0[], na.rm = TRUE) > 0) & ((extreme_offset - D_max_DOY) >5))
      {
        RPS <- which((diff0[] <= 0) & (diff1[] > 0))
        RPE <- which((diff0[] > 0) & (diff1[] <= 0))
        if (length(RPE) < length(RPS)) {
          RPE <- c(RPE,extreme_offset - D_max_DOY + 1)
        }
        RPN <- length(RPS)  # number of rainfall pulses between peak drought day and extreme drought offset
        RPI <- mat.or.vec(RPN, 1)
        for (rp in 1:RPN)
        {
          RPI[rp] <- sum(diff0[(RPS[rp] + 1):(RPE[rp])])
        }
        RPImean <- abs(mean(RPI, na.rm = TRUE))
        RPIsd <- sd(RPI, na.rm = TRUE)
        RPSDOY <- D_max_DOY + 1 + RPS
        RPEDOY <- D_max_DOY + RPE
      } else {
        RPImean = 0
        RPIsd <- 0
        RPN = 0

      }
    }


    # Stocking our extracted Drought Features in the empty table (tabresult)

    drought_features_table[year,1] <- drying_rate
    drought_features_table[year,2] <- wetting_rate
    drought_features_table[year,3] <- intercept_drying_curve
    drought_features_table[year,4] <- intercept_wetting_rate
    drought_features_table[year,5] <- D_max
    drought_features_table[year,6] <- D_max_DOY
    drought_features_table[year,7] <- RPN
    drought_features_table[year,8] <- RPImean
    drought_features_table[year,9] <- RPIsd
    drought_features_table[year,10] <- low_onset
    drought_features_table[year,11] <- mod_onset
    drought_features_table[year,12] <- extreme_onset
    drought_features_table[year,13] <- low_offset
    drought_features_table[year,14] <- mod_offset
    drought_features_table[year,15] <- extreme_offset
    drought_features_table[year,16] <- KBDI_DD_low_50
    drought_features_table[year,17] <- KBDI_DD_mod_100
    drought_features_table[year,18] <- KBDI_DD_extreme_150
    drought_features_table[year,19] <- KBDI_DS_low_50
    drought_features_table[year,20] <- KBDI_DS_mod_100
    drought_features_table[year,21] <- KBDI_DS_extreme_150


    if (plot_features_extraction){

      points (low_onset, KBDI_50_onset, col = 'yellow', pch = 16, cex = 1.3)
      points (mod_onset, KBDI_100_onset, col = 'orange', pch = 16, cex = 1.3)
      points (extreme_onset, KBDI_150_onset, col = 'red', pch = 16, cex = 1.3)
      points (low_offset, KBDI_50_offset, col = 'yellow', pch = 16, cex = 1.3)
      points (mod_offset, KBDI_100_offset, col = 'orange', pch = 16, cex = 1.3)
      points (extreme_offset, KBDI_150_offset, col = 'red', pch = 16, cex = 1.3)
      points( D_max_DOY, D_max, col = 'red', pch = 8)
      points(elmore,type = 'l',col = 'grey')
      points(drying_rate*t+intercept_drying_curve,type = 'l',col='red', lwd = 1.5)
      points(wetting_rate*t+intercept_wetting_rate,type = 'l',col='blue', lwd = 1.5)

      legend("top", inset = c( 0, -0.3),   xpd = TRUE,                # Create legend outside of plot
             legend = c("KBDI - 50"," KBDI - 100     ", 'KBDI - 150', "Fitted Curve", "Peak.KBDI"),
             pch = c(16,16,16,NA, 8),
             col = c( "yellow",'orange', 'red', 'grey', 'red'),
             lty = c(NA,NA, NA,1, NA),
             horiz= F, cex = 0.7,bty = "n", ncol = 2)

    }

    cat("\033[1;31m", 'Fine fitting of the year', start_year + (year-1), 'is over', "\033[0m\n")

    #end
  }


  # If save_to_csv is TRUE, save the result as a CSV file
  if (save_to_csv) {
    write.csv(drought_features_table, file = file_name, row.names = TRUE)
    message(paste("File saved as", file_name))
  }


  return(drought_features_table)

}
