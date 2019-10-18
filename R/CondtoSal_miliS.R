#' Convert Conductivity to Salinity
#' using equations from UNESCO 1983. Taken from James Douglass's Excel sheet
#' @param cond Conductivity in miliSeimens/m^2/s
#' @param temp Temperature in Celsius
#' @example Cond.to.Sal(33.2, 1.2)
Cond.to.Sal<- function (cond,temp) {
  
  
  var1<-cond/42.9 ##this is the conductivity ratio. 42.9 is the reference conductivity in mS/cm. If you measured conductivity in different units, you will need to change the reference conductivity to match.
  
  var2<-0.6766097+(0.0200564*temp)+(0.0001104259*temp^2)+((-6.9698*10^-7)*temp^3)+((1.0031*10^-9)*temp^4) ##rt
  
  var3<-var1/var2 ##Rt
  
  var4<-((temp-15)/(1+(0.0162*(temp-15))))*(0.0005+((-0.0056)*var3^0.5)+((-0.0066)*var3)+((-0.0375)*var3^1.5)+((0.0636)*var3^2)+((-0.0144)*var3^2.5))#dS
  
  Sal<-0.008+((-0.1692)*var3^0.5)+(25.3851*var3)+(14.0941*var3^1.5)+((-7.0261)*var3^2)+(2.7081*var3^2.5)+var4
  
  
  return(Sal)
}
