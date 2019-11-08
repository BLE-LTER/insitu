#' Calibrate temperature and salinity data
#' @param instrument_data (data.frame) Data frame of instrument data (that needs to be calibrated). Required column names are "date_time", "temperature", "conductivity". "date_time" column needs to be in POSIXct format.
#' @param ysi_data (data.frame) Data frame of measurements to calibrate by (often by a YSI). Required column names are "date_time", "temperature", "conductivity". "date_time" column needs to be in POSIXct format.
#'

# ysi data will have a datetime column that gets rounded to the nearest hour? and gets matched up to the instrument date time and then we calibrate by however many rows ysi has

TScal <- function(instrument_data, ysi_data) {

  # first reformat col names
  colnames(ysi_data) <-
    tolower(gsub("\\.", "\\_", colnames(ysi_data)))
  names(ysi_data)[grep("time", names(ysi_data))] <- "date_time"

    # round ysi datetimes to nearest hour
  ysi_data[["date_time"]] <- as.POSIXct(round.POSIXt(ysi_data[["date_time"]], units = "hours"))
  ysi_data <- ysi_data[!is.na(ysi_data[["date_time"]]), ]

  for (i in unique(instrument_data[["station"]])) {

  }
  calib_rows <- which(instrument_data[["date_time"]] %in% ysi_data[["date_time"]])
  return(calib_rows)
  # instrument_data[["temperature_calibrated"]] <- TS.adj(instrument_data[["temperature"]], )
}
