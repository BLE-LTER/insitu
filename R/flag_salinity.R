#' @title Identify and flag invalid ("anomalous") data based on freezing line
#' @description Calculates the salinity error based on the innate loggger conductivity and temperature error margins.
#' Determines if dataset's error margins fall within an acceptable range.
#' Data is "anomylous" if data +C+T error is below the freezing line.
#' @param data (data.frame) Data to be flagged.
#' @param tempcol (character) Name of column for temperature. Defaults to "temperature".
#' @param condcol (character) Name of column for conductivity. Defaults to "conductivity".
#' @param Terror (numeric) Precision for temperature.
#' @param Cerror (numeric) Precision for conductivity.
#' @param flag_scheme (character) Scheme for flagging anomalous salinity values. Defaults to NULL and outputs a logical TRUE/FALSE column. Alternatively supply a vector of two character flags to denote good/bad data respectively, e.g. c("valid", "invalid").
#' @return (data.frame) The input dataframe with new columns added: "anomalous" (logical) indicating anomalous values, "salinity" (named according to input argument salcol) for calculated salinity.
#' @export

flag_salinity <- function(data, tempcol = "temperature", condcol = "conductivity", Terror, Cerror, flag_scheme = NULL) {

    posTerror <- data[[tempcol]] + Terror
  posCerror <- data[[condcol]] + Cerror
  pCpTSalerror <- calculate_salinity(posCerror, posTerror)

  # data is "anomalous" if data +C+T error is below the freezing line
  data[["anomalous"]] <-
    posTerror < (-0.0575 * pCpTSalerror) + (pCpTSalerror^1.5 * 1.710523E-3) - (2.154996E-4 * pCpTSalerror ^ 2) - 7.53E-4

  if (!is.null(flag_scheme)) {
    if (is.character(flag_scheme) && length(flag_scheme) == 2){
      data[data[["anomalous"]], "anomalous"] <- as.character(flag_scheme[1])
      data[!data[["anomalous"]], "anomalous"] <- as.character(flag_scheme[2])
    }
  }

  return(data)
}
