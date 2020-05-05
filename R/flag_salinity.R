#' @title Identify and flag invalid ("anomalous") data based on freezing line
#' @description Calculates the salinity error based on the innate loggger conductivity and temperature error margins.
#' Determines if dataset's error margins fall within an acceptable range.
#' Data is "anomylous" if data +C+T error is below the freezing line.
#' @param data (data.frame) Data to be flagged.
#' @param tempcol (character) Name of column for temperature. Defaults to "temperature".
#' @param condcol (character) Name of column for conductivity. Defaults to "conductivity".
#' @param Terror (numeric) Precision for temperature.
#' @param Cerror (numeric) Precision for conductivity.
#' @param ref_cond (numeric) Reference conductivity value for calculating salinity, defaults to 42.9 for conductivity measured in miliSeimens/m^2/s.
#' @param flag_colname (character) Column name to give the new flag column, defaults to "anomalous".
#' @param flag_scheme (character) Scheme for flagging anomalous salinity values. Supply a vector of character flags to denote good/bad data respectively, e.g. c("valid", "invalid"). The first two will be used. IF NULL function will output a logical TRUE/FALSE column.
#' @return (data.frame) The input dataframe with a new column added, named by the flag_colname argument, defaults to "anomalous" (character or logical) indicating anomalous values.
#' @export

flag_salinity <-
  function(data,
           tempcol = "temperature",
           condcol = "conductivity",
           Terror,
           Cerror,
           ref_cond = 42.9,
           flag_colname = "anomalous",
           flag_scheme = c("valid", "invalid")) {
    stopifnot(is.data.frame(data), is.numeric(Terror), is.numeric(Cerror), is.null(flag_scheme) | length(flag_scheme) >= 2)

    if (!tempcol %in% colnames(data)) stop(paste(tempcol, "not a column in data"))
    if (!condcol %in% colnames(data)) stop(paste(condcol, "not a column in data"))

    posTerror <- data[[tempcol]] + Terror
    posCerror <- data[[condcol]] + Cerror
    pCpTSalerror <- calculate_salinity(posCerror, posTerror, ref_cond)

    # data is "anomalous" if data +C+T error is below the freezing line
    data[[flag_colname]] <-
      posTerror < (-0.0575 * pCpTSalerror) + (pCpTSalerror ^ 1.5 * 1.710523E-3) - (2.154996E-4 * pCpTSalerror ^ 2) - 7.53E-4

    if (!is.null(flag_scheme)) {
      if (is.character(flag_scheme) && length(flag_scheme) == 2) {
        data[as.character(data[[flag_colname]]) == "FALSE", flag_colname] <-
          as.character(flag_scheme[1])
        data[as.character(data[[flag_colname]]) == "TRUE", flag_colname] <-
          as.character(flag_scheme[2])
      }
    }
    return(data)
  }
