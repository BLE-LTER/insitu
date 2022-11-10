#' Identify outliers via point and click on temp vs salinity plot
#' @param data (data.frame) Data frame with columns for date or temperature, salinity.
#' @param datecol (character) Name of colume for date time. Defaults to "date_time".
#' @param tempcol (character) Name of column for temperature. Defaults to "temperature".
#' @param salcol (character) Name of column for salinity. Defaults to "salinity".
#' @param rowseq (numeric) Numeric vector of row numbers starting from one. Defaults to seq(1:nrow(data)).
#' @param plot_type (character) Either "sal/time" for plotting salinity over time, or "temp/sal" for plotting temperature vs salinity. Defaults to "sal/time".
#' @param plot_title (character) Give a plot title. Defaults to "Manually identify anomalous data points by clicking on them. Click stop on the upper left and exit for the function to return row numbers of ID'ed points". This is useful only if you'd like to save the plot as image.
#' @return Numeric vector of row numbers manually ID'ed by user. Note that this returns an error if user did not click stop before exiting the plot application.
#' @export

id_outlier <-
  function(data,
           datecol = "date_time",
           tempcol = "temperature",
           salcol = "salinity",
           rowseq = seq(1:nrow(data)),
           plot_type = "sal/time",
           plot_title = "Manually identify anomalous data points by clicking on them. \n Click stop on the upper left and exit for the function to return row numbers of ID'ed points") {
    my.col <-
      grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(diff(range(rowseq)))
    win.graph(50, 35)

    if (plot_type == "sal/time") {
      plot(
        data[[datecol]],
        data[[salcol]],
        # col = ifelse(data$flag == F, my.col, 'black'),
        col = my.col,
        cex = .8,
        main = plot_title,
        ylim = c(0, 45)
      )
      badpts <-
        identify(data[[datecol]], data[[salcol]], labels = rowseq)
      badpts
    } else if (plot_type == "temp/sal") {
      plot(data[[salcol]],
           data[[tempcol]],
           col = my.col,
           cex = .8,
           main = plot_title)
      badpts <-
        identify(data[[salcol]], data[[tempcol]], labels = rowseq)
      badpts
    }
  }
