#' @title Convert Conductivity to Salinity
#' @description Calculate salinity from conductivitity and temperature, using equations from UNESCO 1983. Taken from James Douglass's Excel sheet.
#' @param cond (numeric) Numeric vector of conductivity values in miliSeimens/m^2/s.
#' @param temp (numeric) Numeric vector of temperature values in degree Celsius.
#' @param ref_cond (numeric) Reference conductivity value, defaults to 42.9 for conductivity measured in miliSeimens/m^2/s.
#' @return (numeric) Vector of salinity values calculated from each pair of cond and temp values in input vectors. Unit varies depend on unit for conductivity, defaults to  PSU.
#' @export

calculate_salinity <- function(cond, temp, ref_cond = 42.9){

  if (length(cond) != length(temp)) error("Please supply vectors of same lengths for cond and sal.")
  if (length(ref_cond) > 1) error("Please supply only one reference conductivity.")
  if (!is.numeric(cond) | !is.numeric(temp) | !is.numeric(ref_cond)) error("Please supply numeric values.")

  # this is the conductivity ratio. 42.9 is the reference conductivity in mS/cm. If you measured conductivity in different units, you will need to change the reference conductivity to match.
  var1 <- cond/ref_cond

  # rt
  var2 <- 0.6766097 + (0.0200564 * temp) + (0.0001104259 * temp^2) + ((-6.9698 * 10^-7) * temp^3) + ((1.0031 * 10^-9) * temp^4)

  # Rt
  var3 <- var1/var2

  # dS
  var4 <- ((temp - 15) / (1 + (0.0162 * (temp - 15)))) * (0.0005 + ((-0.0056) * var3^0.5) + ((-0.0066) * var3) + ((-0.0375) * var3^1.5) + ((0.0636) * var3^2) + ((-0.0144) * var3^2.5))

  Sal <- 0.008 + ((-0.1692) * var3^0.5) + (25.3851 * var3) + (14.0941 * var3^1.5) + ((-7.0261) * var3^2) + (2.7081 * var3^2.5) + var4

  return(Sal)
}
