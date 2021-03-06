% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calibrate_data.R
\name{calibrate_data}
\alias{calibrate_data}
\title{Calibrate instrument data according to referenced points}
\usage{
calibrate_data(instrument_data, ysi_data, cal_by = "value",
  raw = "temperature", calibrated = paste0(raw, "_calibrated"),
  some_missing = TRUE)
}
\arguments{
\item{instrument_data}{(data.frame) Data frame of instrument data (that needs to be calibrated). Required column names are "station", "date_time" in POSIXct format. YYYY-MM-DD hh:mm:ss would be helpful.}

\item{ysi_data}{(data.frame) Data frame of measurements to calibrate by (often by a YSI). Required column names are "station", "date_time". "date_time" in POSIXct format. YYYY-MM-DD hh:mm:ss would be helpful. There's some leeway with column names: "date_time": having a column name with "time" or "DT" in it is sufficient.}

\item{cal_by}{(character) Name of value column in YSI data (data to calibrate by). Defaults to "value". This column and the raw column should be measurements of the same thing, e.g. temperature. Note that if this column name is identical to the raw column name in instrument_data, "_ysi" will be appended to the column name in the output dataframe.}

\item{raw}{(character) Name of value column in instrument data (raw data). Defaults to "temperature".}

\item{calibrated}{(character) Name of column to store output calibrated data. Default to "_calibrated" appended to the raw column name.}

\item{some_missing}{(logical) TRUE/FALSE on whether to calibrate where there are beginning or end dates that do not have calibrated values (e.g. if the instrument was deployed on June 1 but there's no calibration data until July 1). If TRUE, function will assume the instrument data on the first/last dates are true and use them as calibration points. If FALSE, the time portions prior or after the first/last calibration values will not altered. Defaults to TRUE.}
}
\description{
When supplied with a df of instrument data and a df of data to calibrate by (e.g. YSI data), this function matches the two sets of data up by station and datetime (rounded to the nearest hour), then call adjust_linear to apply linear adjustment between however many pairs of calibration points applicable.
}
