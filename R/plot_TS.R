#'  Initial look at Temperature and Salinity data
#' @description Returns figure with plots of temperature vs salinity,  salinity over time, and temperature over time
#' @param Temp (character) column name of temperature values in Celsius
#' @param Sal (character) column name of salinity
#' @param Date (dttm) column name of date or date time
#' @param P (numeric) pressure in atm, default is 1
#' @param plottitle (character) title of plot, default is "T vs S"
#' @return side by side plots, first of temperature vs salinity with freezing line, second of salinity over time, third of temperature over time
#' @examples
#' plot_tempsal(test_data,"Temperature","Salinity","DT", plottitle="KALD1")
#' @importFrom viridisLite viridis
#' @importFrom cowplot ggdraw
#' @export



plot_tempsal <- function(df, Temp, Sal, Date, Pres = 1.3, plottitle = "Temperature vs Salinity"){

  freeze <- function(S,P = Pres){ # create equation for freezing line
    TF <-  (-0.0575 + 1.710523E-3 * sqrt(abs(S)) - 2.154996E-4 * S) * S - 7.53E-4 * (P*10.132501)
    # note: equation is valid in the practical salinity range of 4 to 40 at atmospheric pressure. This version assumes depth of 3 m
  }

  p1 <- ggplot(df, aes_string(x= Sal, y = Temp, color=Date))+
    geom_point()+
    scale_color_gradientn(colors = viridis(100), trans="time")+
    stat_function(fun = freeze)+
    theme(legend.position = "none")

  p2 <- ggplot(df, aes_string(x= Date, y = Sal, color=Date))+
    geom_point()+
    scale_color_gradientn(colors = viridis(100), trans="time")+
    theme(legend.position = "none")

  p3 <- ggplot(df, aes_string(x= Date, y = Temp, color=Date))+
    geom_point()+
    scale_color_gradientn(colors = viridis(100), trans="time")+
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

  plotright <- plot_grid(p2, p3, ncol=1)

  plotrow <- plot_grid(p1, plotright, rel_widths = c(1, 1.5))

  plots <- plot_grid(title, plotrow, ncol = 1, rel_heights = c(0.1, 1))

  return(plots)

}
