#' Linearly adjust data based on two data points of know quality.
#' @param raw (numeric) Numeric vector of raw data, sorted by date time ascending.
#' @param calib (numeric) Numeric vector that is blank except for the calibration values.
#' @param rowfirst (numeric) Rownumber of the first calibration value.
#' @param rowlast (numeric) Rownumber of the second (and last) calibration value.
#' @param rownums (numeric) Vector of row numbers. Defaults to 1:length(raw)
#' @return A vector with caliberated values. Adjustment is linear.
#' @export

adjust_linear <- function(raw, calib, rowfirst, rowlast, rownums = 1:length(raw)) {
  x1 <- calib[rowfirst] - raw[rowfirst]
  x2 <- calib[rowlast] - raw[rowlast]
  xslope <- (x2 - x1) / (rowlast - rowfirst)
  calib <- raw + x1 + ((rownums - 1) * xslope)
  return(calib)
}

