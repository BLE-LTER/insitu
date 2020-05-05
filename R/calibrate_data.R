#' @title Calibrate instrument data according to referenced points
#' @description When supplied with a df of instrument data and a df of data to calibrate by (e.g. YSI data), this function matches the two sets of data up by station and datetime (rounded to the nearest hour), then call adjust_linear to apply linear adjustment between however many pairs of calibration points applicable.
#' @param instrument_data (data.frame) Data frame of instrument data (that needs to be calibrated). Required column names are "station", "date_time" in POSIXct format. YYYY-MM-DD hh:mm:ss would be helpful.
#' @param ysi_data (data.frame) Data frame of measurements to calibrate by (often by a YSI). Required column names are "station", "date_time". "date_time" in POSIXct format. YYYY-MM-DD hh:mm:ss would be helpful. There's some leeway with column names: "date_time": having a column name with "time" or "DT" in it is sufficient.
#' @param cal_by (character) Name of value column in YSI data (data to calibrate by). Defaults to "value". This column and the raw column should be measurements of the same thing, e.g. temperature. Note that if this column name is identical to the raw column name in instrument_data, "_ysi" will be appended to the column name in the output dataframe.
#' @param raw (character) Name of value column in instrument data (raw data). Defaults to "temperature".
#' @param calibrated (character) Name of column to store output calibrated data. Default to "_calibrated" appended to the raw column name.
#' @param some_missing (logical) TRUE/FALSE on whether to calibrate where there are beginning or end dates that do not have calibrated values (e.g. if the instrument was deployed on June 1 but there's no calibration data until July 1). If TRUE, function will assume the instrument data on the first/last dates are true and use them as calibration points. If FALSE, the time portions prior or after the first/last calibration values will not altered. Defaults to TRUE.
#' @importFrom lubridate force_tz
#' @importFrom crayon bold green blue red
#' @export

calibrate_data <-
  function(instrument_data,
           ysi_data,
           cal_by = "value",
           raw = "temperature",
           calibrated = paste0(raw, "_calibrated"),
           some_missing = TRUE) {

    # check for dfs
    if (!is.data.frame(instrument_data))
      stop("instrument_data not data.frame")
    if (!is.data.frame(ysi_data))
      stop("ysi_data not data.frame")
    if (!cal_by %in% names(ysi_data))
      stop(paste(cal_by, "not a column in ysi_data"))

    # check for expected cols in ysi
    if (!any(grepl("time|dt", names(ysi_data), ignore.case = TRUE)))
      stop("no columns vaguely resembling date_time detected in ysi_data")
    if (!"station" %in% names(instrument_data))
      stop("station not a column in ysi_data")

    # check for expected cols in instrument
    if (!raw %in% names(instrument_data))
      stop(paste(raw, "not a column in instrument_data"))
    if (!"station" %in% names(instrument_data))
      stop("station not a column in instrument_data")
    if (!"date_time" %in% names(instrument_data))
      stop("date_time not a column in instrument_data")

    # checks are complete, proceed to do stuff
    # first reformat col names in ysi to all lowercase and do grep to make sure columns are named "station" and "date_time" exactly
    colnames(ysi_data) <-
      tolower(gsub("\\.", "\\_", colnames(ysi_data)))
    names(ysi_data)[grep("time|dt", names(ysi_data), ignore.case = TRUE)] <-
      "date_time"
    cal_by <- tolower(cal_by)

    # in case raw and cal_by are the same
    if (raw == cal_by) {
      colnames(ysi_data)[[which(colnames(ysi_data) == cal_by)]] <-
        paste0(cal_by, "_ysi")
      cal_by <- paste0(cal_by, "_ysi")
    }

    # round ysi datetimes to nearest hour, force to standard Alaska timezone
    ysi_data[["date_time"]] <-
      force_tz(round.POSIXt(ysi_data[["date_time"]], units = "hours"), "Etc/GMT+8")
    ysi_data <- ysi_data[!is.na(ysi_data[["date_time"]]),]
    # do the same for instrument data
    instrument_data[["date_time"]] <- force_tz(as.POSIXct(instrument_data[["date_time"]]), "Etc/GMT+8")

    # merge instrument and ysi data, keeping all instrument data points, only keeping relevant YSI columns
    merged <-
      merge(
        instrument_data,
        ysi_data[c("station", "date_time", cal_by)],
        by = c("station", "date_time"),
        all.x = TRUE
      )
    # reorder by station then date, since we're calibrating by row order
    merged <-
      merged[order(merged[["station"]], merged[["date_time"]]),]

    # initiate output df
    output <- data.frame()

    # loop by station
    for (i in unique(instrument_data[["station"]])) {
      by_station <- subset(merged, station == i)
      raw_col <- by_station[[raw]]
      cal_by_col <- by_station[[cal_by]]
      by_station[[calibrated]] <- raw_col

      # get the datetimes with calibrated values
      cal_by_rows <- which(!is.na(cal_by_col))
      num_rows <- nrow(by_station)

      # output messages
      if (length(cal_by_rows) > 0) {
        message(
          paste(
            bold(i),
            "station:",
            "\n\tFound",
            bold(length(cal_by_rows)),
            "calibration point(s) from YSI data",
            "at:",
            blue(paste0(by_station[cal_by_rows, "date_time", drop = T], collapse = ", "))
          )
        )
        # copy over first/last if some_missing == TRUE and add first/last to calibrated points
        if (some_missing) {
          if (is.na(cal_by_col[1])) {
            message(paste(
              green(
                "\tUsing first row of raw instrument data at",
                blue(by_station[[1, "date_time"]]),
                "as calibration point."
              )
            ))
            cal_by_col[1] <- raw_col[1]
            cal_by_rows <- c(1, cal_by_rows)
          }
          if (is.na(cal_by_col[num_rows])) {
            message(paste(
              green(
                "\tUsing last row of raw instrument data at",
                blue(by_station[[num_rows, "date_time"]]),
                "as calibration point."
              )
            ))
            cal_by_col[num_rows] <- raw_col[num_rows]
            cal_by_rows <- c(cal_by_rows, num_rows)
          }
        }

        # loop over the number of calibration points
        for (m in 1:(length(cal_by_rows) - 1)) {
          cal_start <- cal_by_rows[m]
          cal_end <- cal_by_rows[m + 1]

          # call adjust_linear. note that since we'll calling it on a subset of raw and calib, index gets reset and rowfirst needs to be 1
          by_station[[calibrated]][cal_start:cal_end] <-
            adjust_linear(
              raw = raw_col[cal_start:cal_end],
              calib = cal_by_col[cal_start:cal_end],
              rowfirst = 1,
              rowlast = cal_end - cal_start + 1,
              rownums = 1:(cal_end - cal_start + 1)
            )
        } # end calibration points for loop
      } # end if found > 0 calibration point

      else if (length(cal_by_rows) > 0)
        message(red(paste(
          "Found 0 calibration point for station",
          bold(i),
          "so function will not alter raw data."
        )))
      # append data to output df
      output <- rbind(output, by_station)
    } # end station for loop
    return(output)
  }
