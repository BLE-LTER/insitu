####determine anomolous TS data using instrument error
####made for BP TempSal workup fall 2017.R
####adapted for BLE LTER
####Christina Bonsell
####last edit Oct 2019



#source('scr/CondtoSal_miliS.R')

#print("SO error: T=0.1, Cond=1.5 mS/cm, RBR error: T=0.002, Cond=0.01 mS/cm")

#' Identify invalid ("anom") data based on freezing line
#' calculates the salinity error based on the innate loggger cond and temp error margins (Terror and Cerror)
#' determines if dataset's error margins fall within an acceptable range
#' data is "anomylous" if data +C+T error is below the freezing line
#' @param dat dataframe with columns for temperature and conductivity
#' @param tempcol name of column for temperature, in quotes
#' @param condcol name of column for conductivity, in quotes
#' @param Terror precision for temperature
#' @param Cerror precision for conductivity
#' @example TSanom_workup(RBR, "Temperature", "Conductivity", .002,.01)

TSanom_workup <- function(dat, tempcol, condcol, Terror, Cerror) {
  dat$Sal <- Cond.to.Sal(dat[[condcol]], dat[[tempcol]])


  dat$posTerror <- dat[[tempcol]] + Terror
  dat$negTerror <- dat[[tempcol]] - Terror

  dat$posCerror <- dat[[condcol]] + Cerror
  dat$negCerror <- dat[[condcol]] - Cerror

  dat$pCpTSalerror <- Cond.to.Sal(dat$posCerror, dat$posTerror)
  dat$pCnTSalerror <- Cond.to.Sal(dat$posCerror, dat$negTerror)
  dat$nCpTSalerror <- Cond.to.Sal(dat$negCerror, dat$posTerror)
  dat$nCnTSalerror <- Cond.to.Sal(dat$negCerror, dat$negTerror)

  #data is "anomylous" if data +C+T error is below the freezing line
  dat$anom <-
    dat$posTerror < (-0.0575 * dat$pCpTSalerror) + ((dat$pCpTSalerror ^ (3 /
                                                                           2)) * 1.710523E-3) - (2.154996E-4 * dat$pCpTSalerror ^ 2) - 7.53E-4

  rownames(dat) <- seq(length = nrow(dat))
  dat$row <- seq(length = nrow(dat))

  dat
}



#' Visulaize the outliers (black) in plot over time
#' @param dat dataframe that has gone through TSanom_workup
#' @param tempcol name of column for temperature, in quotes
#' @param name name of plot, usually site name

outlierTS <- function(dat, tempcol, name) {
  tks <- seq(dat$Date[1], dat$Date[nrow(dat)], by = "months")
  tklb <- format(tks, format = "%b%d")

  my.col <-
    colorRampPalette(brewer.pal(11, "Spectral"))(diff(range(dat$row)))

  win.graph(30, 35)
  par(las = 2)
  par(mfrow = c(2, 1))
  plot(
    dat$Date,
    dat[[tempcol]],
    col = ifelse(dat$anom == F, my.col, 'black'),
    pch = "o",
    cex = .3,
    main = name,
    xaxt = "n"
  )
  axis.POSIXct(1, at = tks, "%m %d", labels = tklb)
  legend("bottomleft", legend = ("Temp"))
  plot(
    dat$Date,
    dat$Sal,
    col = ifelse(dat$anom == F, my.col, 'black'),
    pch = "o",
    cex = .3,
    xaxt = "n"
  )
  axis.POSIXct(1, at = tks, "%m %d", labels = tklb)
  legend("bottomleft", legend = ("Sal"))

}

#' Identify outliers via point and click on salinity over time plot
#' @param dat dataframe with columns for date or datetime, salinity, and row (has to be called row)
#' @param datename name of column for date, in quotes
#' @param salname name of column for salinity, in quotes
#' @param dfname plot title

IDoutlier <- function(dat, datename, salname, dfname) {
  my.col <-
    colorRampPalette(brewer.pal(11, "Spectral"))(diff(range(dat$row)))
  df <- as.data.frame(dat)
  win.graph(50, 35)
  plot(
    df[[datename]],
    df[[salname]],
    col = ifelse(dat$flag == F, my.col, 'black'),
    cex = .8,
    main = dfname,
    ylim = c(0, 45)
  )
  badpts <-
    identify(df[[datename]], df[[salname]], labels = row.names(df))
  badpts
}


#' Identify outliers via point and click on temp vs salinity plot
#' @param dat dataframe with columns for date or temperature, salinity, and row (has to be called row)
#' @param tempname name of column for temperature, in quotes
#' @param salname name of column for salinity, in quotes
#' @param dfname plot title

IDoutlier2 <- function(dat, tempname, salname, dfname) {
  my.col <-
    colorRampPalette(brewer.pal(11, "Spectral"))(diff(range(dat$row)))
  df <- as.data.frame(dat)
  win.graph(50, 35)
  plot(df[[salname]],
       df[[tempname]],
       col = my.col,
       cex = .8,
       main = dfname)
  badpts <-
    identify(df[[salname]], df[[tempname]], labels = row.names(df))
  badpts
}

