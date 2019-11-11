#' Calibrate temperature and salinity data
#' @param instrument_data (data.frame) Data frame of instrument data (that needs to be calibrated). Required column names are "date_time", "temperature", "conductivity". "date_time" column needs to be in POSIXct format.
#' @param ysi_data (data.frame) Data frame of measurements to calibrate by (often by a YSI). Required column names are "date_time", "temperature", "conductivity". "date_time" column needs to be in POSIXct format.
#' @param station_colname (character) Name of station code column in YSI data. Defaults to "station".
#' @param calibrate_by (character) Name of value column in YSI data (data to calibrate by). Defaults to "value". This column and the raw column should be measurements of the same thing, e.g. temperature.
#' @param raw (character) Name of value column in instrument data (raw data). Defaults to "temperature".
#' @param calibrated (character) Name of column to store calibrated data. Default to "_calibrated" appended to the raw column.
#' @param some_missing (logical) T/F on whether to calibrate where there are beginning or end dates that do not have calibrated values (e.g. if the instrument was put down on June 1 but there's no calibration data until July 1). If TRUE, function will assume the instrument data on the first/last dates are true and use them as calibration points. If FALSE, the time portions prior or after the first/last calibration values will not altered. Defaults to TRUE.
#' @importFrom lubridate force_tz as.POSIXct round.POSIXct

# ysi data will have a datetime column that gets rounded to the nearest hour? and gets matched up to the instrument date time and then we calibrate by however many rows ysi has

TScal <- function(instrument_data, ysi_data, station_colname = "station", calibrate_by = "value", raw = "temperature", calibrated = paste0(raw, "_calibrated"), some_missing = TRUE) {

  # first reformat col names in ysi to make sure they are all lowercase
  colnames(ysi_data) <-
    tolower(gsub("\\.", "\\_", colnames(ysi_data)))
  names(ysi_data)[grep("time", names(ysi_data))] <- "date_time"

    # round ysi datetimes to nearest hour, force to standard Alaska timezone
  ysi_data[["date_time"]] <- force_tz(as.POSIXct(round.POSIXt(ysi_data[["date_time"]], units = "hours")), "Etc/GMT+8")
  ysi_data <- ysi_data[!is.na(ysi_data[["date_time"]]), ]
  # do the same for instrument data
  # instrument_data[["date_time"]] <- force_tz(as.POSIXct(instrument_data[["date_time"]]), "Etc/GMT+8")

  # merge instrument and ysi data
  merged <- merge(instrument_data, ysi_data, by = c("station", "date_time"), all.x = TRUE)
  # return(merged)
  # initiate
  calibrated_df <- data.frame()

  # process by station
  for (i in unique(instrument_data[["station"]])) {
    by_station <- subset(merged, station == i)
    # return(by_station[["date_time"]])
    by_station <- by_station[order(by_station[["date_time"]]), ]
    by_station[[calibrated]] <- by_station[[calibrate_by]]
    # return(by_station[[calibrated]])

    # get the datetimes with calibrated values
    calibrated_rows <- which(!is.na(by_station[[calibrate_by]]))
    num_rows <- nrow(by_station)

    # copy over first/last if some_missing == TRUE and add first/last to calibrated points
    if (some_missing) {
      if (is.na(by_station[[calibrate_by]][1])) {
        by_station[[calibrate_by]][1] <- by_station[[raw]][1]
        calibrated_rows <- c(1, calibrated_rows)
      }
      if (is.na(by_station[[calibrate_by]][num_rows])) {
        by_station[[calibrate_by]][num_rows] <- by_station[[raw]][num_rows]
        calibrated_rows <- c(calibrated_rows, num_rows)
      }
    }

    m <- 1
    return(calibrated_rows)


    while (m < length(calibrated_rows)) {
      rowfirst <- calibrated_rows[m]
      rowlast <- calibrated_rows[m + 1]
      by_station[[calibrated]][rowfirst:rowlast] <-
        TS.adj(
          raw = by_station[[raw]][rowfirst:rowlast],
          calib = by_station[[calibrate_by]][rowfirst:rowlast],
          rowfirst = rowfirst,
          rowlast = rowlast,
          rownums = rowfirst:rowlast
        )
      m <- m + 1
    } # end while loop
    calibrated_df <- rbind(calibrated_df, by_station)
  } # end for loop

  return(calibrated_df)
}
