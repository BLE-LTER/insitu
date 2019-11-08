#' Calibrate data
#' Based on initial calibration measurment and end calibration measurement. Adjustment is linear.
#' @param raw (numeric) Numeric vector of raw data.
#' @param calib (numeric) df column that is blank except for the calibration values
#' @param rowfirst (numeric) the rownumber of the first calibration value
#' @param rowlast (numeric) the rownumber of the second (and last) calibration value
#' @param rownums a column of row numbers.
#' @return A vector with caliberated values. Adjustment is linear.
#' @export

TS.adj <- function (raw, calib, rowfirst, rowlast, rownums) {

  x1 <- calib[rowfirst] - raw[rowfirst]
  x2 <- calib[rowlast] - raw[rowlast]
  xslope <- (x2 - x1) / (rowlast - rowfirst)
  calib <- raw + x1 + ((rownums - 1) * xslope)
  return(calib)
}

