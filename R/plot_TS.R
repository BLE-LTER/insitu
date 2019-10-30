#' @title Convert Conductivity to Salinity
#' @description
#' @param Temp (name) column of temperature values in Celsius
#' @param Sal (name) column of salinity in PSU
#' @param Date (Date/Time/Datetime)
#' @param P (numeric) pressure in atm, default is 1
#' @return side by side plots, first of salinity over time, second of temperature vs salinity with freezing line
#' @example plot.TS(test_data,"Temperature","Salinity","DT", plottitle="KALD1")
#' @export
#'

plot.TS <- function(df, Temp, Sal, Date, Pres = 1.3, plottitle){

  freeze <- function(S,P = Pres){ # create equation for freezing line
    TF <-  (-0.0575 + 1.710523E-3 * sqrt(abs(S)) - 2.154996E-4 * S) * S - 7.53E-4 * (P*10.132501)
    # note: equation is valid in the practical salinity range of 4 to 40 at atmospheric pressure. This version assumes depth of 3 m
  }

  p1 <- ggplot(df, aes_string(x= Sal, y = Temp, color=Date))+
    geom_point()+
    scale_color_gradientn(colors = rainbow(100), trans="time")+
    stat_function(fun = freeze)+
    theme(legend.position = "none")

  p2 <- ggplot(df, aes_string(x= Date, y = Sal, color=Date))+
    geom_point()+
    scale_color_gradientn(colors = rainbow(100), trans="time")+
    theme(legend.position = "none")

  title <- ggdraw() +
    draw_label(
      plottitle,
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      plot.margin = margin(0, 0, 0, 7)
    )
  plotrow <- plot_grid(p1, p2, rel_widths = c(1, 1.5))

  plots <- plot_grid(title, plotrow, ncol = 1, rel_heights = c(0.1, 1))

  return(plots)

}




