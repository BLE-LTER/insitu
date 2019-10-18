####TS data calibration functions
####Christina Bonsell
####Oct 2019



#' Calibrate data
#' Based on initial calibration measurment and end calibration measurement
#' Adjustment is linear
#' @param raw df column of raw data
#' @param calib df column that is blank except for the calibration values
#' @param rowfirst the rownumber of the first calibration value
#' @param rowlast the rownumber of the second (and last) calibration value
#' @param rownums a column of row numbers
#' @return calibrated column calib
#' @export

TS.adj <- function (raw, calib, rowfirst, rowlast, rownums) {
  x1 <- calib[rowfirst] - raw[rowfirst]
  x2 <- calib[rowlast] - raw[rowlast]
  xslope <- (x2 - x1) / (rowlast - rowfirst)
  calib <- raw + x1 + ((rownums - 1) * xslope)

  return(calib)
}


#' Calibrate StarOddi data
#' Based on initial calibration measurment and end calibration measurement
#' @param dat a dataframe with rows:Cond_mspercm, Temp_C, fixedCond, fixedTemp
#' @return calibrated dataframe
#' @export

TScal_SO <- function(dat) {
  dat$row <- c(1:nrow(dat))
  dat$fixedTemp <-
    TS.adj(dat$Temp_C, dat$fixedTemp, 1, nrow(dat), dat$row)
  dat$fixedCond <-
    TS.adj(dat$Cond_mspercm, dat$fixedCond, 1, nrow(dat), dat$row)

  dat
}

#' Calibrate RBR data
#' Based on initial calibration measurment and end calibration measurement
#' @param dat is a dataframe with rows:Conductivity, Temperature, fixedCond, fixedTemp
#' @return calibrated dataframe
#' @export

TScal_RBR <- function(dat) {
  dat$row <- c(1:nrow(dat))
  dat$fixedTemp <-
    TS.adj(dat$Temperature, dat$fixedTemp, 1, nrow(dat), dat$row)
  dat$fixedCond <-
    TS.adj(dat$Conductivity, dat$fixedCond, 1, nrow(dat), dat$row)

  dat
}

#' Calibrate StarOddi data with midpoint
#' Based on initial calibration measurment, a midpoint measurement, and end calibration measurement
#' @param dat a dataframe with rows:Cond_mspercm, Temp_C, fixedCond, fixedTemp
#' @param midpoint the row number of the midpoint calibration
#' @return calibrated dataframe
#' @export

TSmidcal_SO <- function(dat, midpoint) {
  ln <- nrow(dat)

  dat$row <- c(1:nrow(dat))
  dat$fixedCond[1:midpoint] <-
    TS.adj(dat$Cond_mspercm, dat$fixedCond, 1, midpoint, dat$row)
  dat$fixedTemp[1:midpoint] <-
    TS.adj(dat$Temp_C, dat$fixedTemp, 1, midpoint, dat$row)
  lrow <- ln - midpoint + 1
  dat$row[midpoint:ln] <- c(1:lrow)
  dat$fixedCond[midpoint:ln] <-
    TS.adj(dat$Cond_mspercm[midpoint:ln], dat$fixedCond[midpoint:ln], 1, lrow, dat$row)
  dat$fixedTemp[midpoint:ln] <-
    TS.adj(dat$Temp_C[midpoint:ln], dat$fixedTemp[midpoint:ln], 1, lrow, dat$row)

  dat
}


#' Calibrate RBR data with midpoint
#' Based on initial calibration measurment, a midpoint measurement, and end calibration measurement
#' @param dat is a dataframe with rows:Conductivity, Temperature, fixedCond, fixedTemp
#' @param midpoint the row number of the midpoint calibration
#' @return calibrated dataframe
#' @export

TSmidcal_RBR <- function(dat, midpoint) {
  ln <- nrow(dat)

  dat$row <- c(1:nrow(dat))
  dat$fixedCond[1:midpoint] <-
    TS.adj(dat$Conductivity, dat$fixedCond, 1, midpoint, dat$row)
  dat$fixedTemp[1:midpoint] <-
    TS.adj(dat$Temperature, dat$fixedTemp, 1, midpoint, dat$row)
  lrow <- ln - midpoint + 1
  dat$row[midpoint:ln] <- c(1:lrow)
  dat$fixedCond[midpoint:ln] <-
    TS.adj(dat$Conductivity[midpoint:ln], dat$fixedCond[midpoint:ln], 1, lrow, dat$row)
  dat$fixedTemp[midpoint:ln] <-
    TS.adj(dat$Temperature[midpoint:ln], dat$fixedTemp[midpoint:ln], 1, lrow, dat$row)

  dat
}
