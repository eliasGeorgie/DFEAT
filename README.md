# DFEAT

<!-- badges: start -->

<!-- badges: end -->

Agro-environmental impacts are more closely linked to the multifaceted temporal patterns of soil water desiccation,
surpassing the confines of a singular drought feature of duration or severity,
hardly yet conceptualized and standardized in a single tool for intercomparison between studies.
We describe here the application of a new tool, DFEAT, to compute a diverse set of drought features,
utilizing a daily soil water content time series standardized here using the empirical Keetch-Byram Drought Index (KBDI).
More particularly, the extracted and characterized features are related to the duration, severity, onset, offset, drying and wetting rates, driest peak day, and rainfall pulses of each drought event.
These features are assessed across three soil desiccation thresholds, which represent varying levels of drought propagation in soil moisture, selected for their potential impact on agro-ecosystems.


The package contains nine user-friendly functions, each clearly described and documented, intended for sequential implementation.
It requires a single input table, a daily climate dataset with columns for 'year', 'month', 'day',
daily precipitation ('prec'), and daily maximum temperature ('Tmax'). When the climate data is loaded,
the initial set of functions verifies the variable names in the specified columns, allowing for additional columns if present but notifying the user.
These functions also ensure that the dataset contains complete years in correct chronological order without any missing values. Once verified,
the subsequent functions, including KBDI calculation and related features extraction functions, can be implemented sequentially.

The package has been developed using the Mediterranean-calibrated version of the KBDI and has been successfully tested across various Mediterranean countries,
yielding reliable results. However, it should be noted that applying this package outside of Mediterranean conditions
may require adjustments to the defined methodological approach to ensure accuracy and relevance in differing climates.



## Installation

You can install the development version of DFEAT from [GitHub](https://github.com/eliasGeorgie/DFEAT.git) with:

``` r
install_github('https://github.com/eliasGeorgie/DFEAT.git')
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(DFEAT)
## basic example code
```
