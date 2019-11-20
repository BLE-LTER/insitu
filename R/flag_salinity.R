#' @title Identify and flag invalid ("anomalous") data based on freezing line
#' @description Calculates the salinity error based on the innate loggger conductivity and temperature error margins.
#' Determines if dataset's error margins fall within an acceptable range.
#' Data is "anomylous" if data +C+T error is below the freezing line.
#' @param data (data.frame) Data to be flagged.
#' @param tempcol (character) Name of column for temperature. Defaults to "temperature".
#' @param condcol (character) Name of column for conductivity. Defaults to "conductivity".
#' @param Terror (numeric) Precision for temperature.
#' @param Cerror (numeric) Precision for conductivity.
#' @param flag_scheme (character) Scheme for flagging anomalous salinity values. Supply a vector of character flags to denote good/bad data respectively, e.g. c("valid", "invalid"). The first two will be used. IF NULL function will output a logical TRUE/FALSE column.
#' @return (data.frame) The input dataframe with a new column added: "anomalous" (character or logical) indicating anomalous values.
#' @export

flag_salinity <-
  function(data,
           tempcol = "temperature",
           condcol = "conductivity",
           Terror,
           Cerror,
           flag_scheme = c("valid", "invalid")) {
    if (!tempcol %in% names(data)) stop(paste(tempcol, "not a column in data"))
    if (!condcol %in% names(data)) stop(paste(condcol, "not a column in data"))
    stopifnot(is.numeric(Terror), is.numeric(Cerror), is.null(flag_scheme) | length(flag_scheme) >= 2)

    posTerror <- data[[tempcol]] + Terror
    posCerror <- data[[condcol]] + Cerror
    pCpTSalerror <- calculate_salinity(posCerror, posTerror)

    # data is "anomalous" if data +C+T error is below the freezing line
    data[["anomalous"]] <-
      posTerror < (-0.0575 * pCpTSalerror) + (pCpTSalerror ^ 1.5 * 1.710523E-3) - (2.154996E-4 * pCpTSalerror ^ 2) - 7.53E-4

    if (!is.null(flag_scheme)) {
      if (is.character(flag_scheme) && length(flag_scheme) == 2) {
        data[as.character(data[["anomalous"]]) == "FALSE", "anomalous"] <-
          as.character(flag_scheme[1])
        data[as.character(data[["anomalous"]]) == "TRUE", "anomalous"] <-
          as.character(flag_scheme[2])
      }
    }
    return(data)
  }
