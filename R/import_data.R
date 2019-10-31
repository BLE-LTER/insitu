#' Import data from instrument-generated files.
#' @param file_list (character) Character vector of file paths. Use list.files() with full.names set to TRUE for this.
#' @param data_type (character) Name of data type. Processing will differ according to data type. Accepted input: "CTD" for conductivity-temperature-depth, or "TCM" for tilt current meter.
#' @param instrument_type (character) Name of instrument (only applies for "CTD" data type). Processing will differ according to instrument. Accepted input: "RBR", "SO".
#' @param param_type (character) Name of parameter type (only applies for "TCM" data type). Processing will differ according to parameter. Accepted inputs: "temp", "current".
#' @param column_names (character) Optional.
#'
#' @importFrom purrr map_df
#' @importFrom readr read_table2
#' @importFrom readxl read_excel
#' @importFrom stringr str_extract
#' @importFrom lubridate as_datetime dmy_hms

import_data <-
  function(file_list,
           data_type,
           instrument_type,
           param_type,
           column_names = c("Temperature", "Conductivity", "Salinity")) {
    # name the vector
    names(file_list) <- file_list

    if (data_type == "CTD") {
      if (instrument_type == "RBR") {
        ########################
        # do RBR import things #
        ########################
        data <- map_df(file_list,
                       read_excel,
                       col_names = TRUE,
                       .id = "file_name")
        data <- data.frame(
          station = str_extract(data[["file_name"]], station_code_pattern),
          date_time = dmy_hms(data[["Timestamp"]], tz = "Etc/GMT+8"), # note: positive sign here actually means places behind Greenwich (west). this produces a negative -8 offset in the actual values
          data[3:11]
        )
        # reformat column names to lowercase and underscores for spaces
        colnames(data) <-
          tolower(gsub("\\.", "\\_", colnames(data)))
        return(data)

      } else if (instrument_type == "SO") {
        ##############################
        # do Star Oddi import things #
        ##############################
        data <-
          map_df(
            file_list,
            read_table2,
            col_names = FALSE,
            .id = "file_name",
            col_types = cols(
              X4 = col_character(),
              X5 = col_character(),
              X6 = col_character(),
              X7 = col_character()
            )
          )
        # replace the decimal comma with the decimal period
        data[5:8] <-
          lapply(data[5:8], function(x)
            sub(",", ".", x, fixed = TRUE))
        data <- data.frame(
          station = str_extract(data[["file_name"]], station_code_pattern),
          date_time = as_datetime(paste(
            as.Date(data[["X2"]], format = "%d.%m.%Y"), data[["X3"]]
          )),
          temperature = data[["X4"]],
          salinity = data[["X5"]],
          conductivity = data[["X6"]],
          sound_velocity = data[["X7"]]
        )
        return(data)

      } else
        error(
          paste0(
            "Instrument type ",
            instrument_type,
            "not supported. Please enter a supported instrument type."
          )
        )

    } else if (data_type == "TCM") {
      ########################
      # do TCM import things #
      ########################
      if (param_type == "temp") {
        data <- map_df(file_list,
                       read_csv,
                       .id = "file_name")

        data <- data.frame(
          station = str_extract(data[["file_name"]], station_code_pattern),
          date_time = data[["ISO 8601 Time"]],
          temperature = data[["Temperature (C)"]]
        )
        return(data)

      } else if (param_type == "current") {
        data <- map_df(file_list,
                       read_csv,
                       .id = "file_name")
        data <- data.frame(
          station = str_extract(data[["file_name"]], station_code_pattern),
          date_time = data[["ISO 8601 Time"]],
          speed = data[["Speed (cm/s)"]],
          heading = data[["Heading (degrees)"]],
          velocity_north = data[["Velocity-N (cm/s)"]],
          velocity_east = data[["Velocity-E (cm/s)"]]
        )
        return(data)

      } else
        error(
          paste0(
            "Parameter type ",
            param_type,
            "not supported. Please enter a supported instrument type."
          )
        )
    } else
      error(paste0(
        "Data type ",
        data_type,
        " not supported. Please enter a supported data type."
      ))
  }

station_codes <- c(
  "EELD1",
  "EELD2",
  "EELS2",
  "EELS1",
  "EWLS2",
  "EWLD1",
  "EWLS1",
  "EWLD2",
  "STLD2",
  "STLD1",
  "SILD2",
  "SILD1",
  "KALD1",
  "KALD2",
  "JALD2",
  "JALD1",
  "KALS1",
  "KALS2",
  "JALS1",
  "JALS2",
  "JAR1",
  "AVR1",
  "NNR1",
  "MYR1",
  "SAR1",
  "KUR1",
  "STLS1",
  "STLS2",
  "SILS1",
  "SILS2"
)

station_code_pattern <-
  paste(station_codes[order(station_codes)], collapse = "|")
