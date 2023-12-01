###Proyecciones a Futuro 
install.packages("forecast")
install.packages("ggplot2")
library(forecast)
library(ggplot2)

###Funcion temporal para eliminar la primera y ultima linea 
gold <-gold[-c(1, nrow(gold)),] 
###

serie_temporal <- ts(gold$Future, frequency = 35)

modelo_arima <- auto.arima(serie_temporal)

proyecciones <- forecast(modelo_arima, h = 24)

print(proyecciones)

plot(proyecciones, main = "Proyección de Precios Forward", xlab = "Fecha", ylab = "Precio Forward")
lines(serie_temporal, col = "blue")
